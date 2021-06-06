#!/usr/bin/perl -w
# ---------------------------------------------------------------------------------------------- #
# Program   swaggercodegen.pl                                                                    #
#                                                                                                #
# Author    Axel Zuber                                                                           #
# Created   08.03.2018                                                                           #
#                                                                                                #
# Description   for all api's from the swagger file(s) listed on the command line :              #
#                                                                                                #
#               - generate model, controller and service classes                                 #
#               - generate model class instantiation code                                        #
#               - resolve references to local and remote external files                          #
#               - print a structured report of endpoints from input file                         #
#               - print data types parsed from input file                                        #
#                                                                                                #
# Supported Open API version : 3.0                                                               #
#                                                                                                #
# Target Environment         : Spring Boot 2.4                                                   #
#                                                                                                #
# Exit code                                                                                      #
#        0 : Successful                                                                          #
#        4 : Warnings                                                                            #
#        8 : Errors                                                                              #
#                                                                                                #
# ---------------------------------------------------------------------------------------------- #
#                                                                                                #
# Prerequisite perl packages :                                                                   #
#                                                                                                #
# Cwd                        HTTP::Date                                                          #
# Digest::MD5                HTTP::Request                                                       #
# Encode                     HTTP::Response                                                      #
# Encode::Locale             JSON::PP                                                            #
# File::Basename             LWP::MediaTypes                                                     #
# File::Path                 LWP::UserAgent                                                      #
# File::RelDir               Storable                                                            #
# File::Spec                 URI                                                                 #
# Getopt::Std                YAML::XS                                                            #
#                                                                                                #
# ---------------------------------------------------------------------------------------------- #
#                                                                                                #
# History                                                                                        #
#                                                                                                #
# Ver   Date        Name        Description                                                      #
#                                                                                                #
# 1.0   08.03.2018  A. Zuber    initial version                                                  #
# 2.0   01.06.2021  A. Zuber    implement Open API 3.0                                           #
#                                                                                                #
# ---------------------------------------------------------------------------------------------- #

package MAIN;

use strict;
use warnings;
use open ":std", ":encoding(UTF-8)";
use vars qw($scriptName $scriptDir);

# ------------------------------------------------------------------------------------ #
# used packages                                                                        #
# ------------------------------------------------------------------------------------ #

use File::Basename;
use Getopt::Std;

########################################################################
##
# initialization (CAVEAT : BEGIN is executed at compile! time)
##

BEGIN
{
    $scriptName = basename($0);
    $scriptDir  = dirname($0);
}

# ------------------------------------------------------------------------------------ #
# global constants                                                                     #
# ------------------------------------------------------------------------------------ #

use constant swagger_version => 3.0;

my $inbuilttypes = {
      'string'  => 'String'
    , 'number'  => 'BigDecimal'
    , 'integer' => 'BigInteger'
    , 'boolean' => 'Boolean'
    , 'file'    => 'String'
    , 'object'  => 'Object'
    , 'void'    => 'Void'
    , 'null'    => 'Void'
};

my $readInput = {
    'json' => \&FileInterface::readJSON,
    'yaml' => \&FileInterface::readYAML,
};

my $configoptions               = {};
my $inputhash                   = {};
my $mergedfiles                 = {};
my $symboltable                 = {};
my $collectedEndpoints          = {};
my $taggedEndpoints             = {};
my $collectedClassInformation   = {};
my $collectedClassMethodIndex   = {};
my $classesSuppressedFromImport = {};

my $invocationPad               = ' ' x 15;

##
#   this mapping table is generated from
#   org.springframework.http.HttpStatus
##
my $mapResponseCodeToEnum =
{
    '100' => 'CONTINUE',                      '101' => 'SWITCHING_PROTOCOLS',             '102' => 'PROCESSING',
    '103' => 'CHECKPOINT',
    '200' => 'OK',                            '201' => 'CREATED',                         '202' => 'ACCEPTED',
    '203' => 'NON_AUTHORITATIVE_INFORMATION', '204' => 'NO_CONTENT',                      '205' => 'RESET_CONTENT',
    '206' => 'PARTIAL_CONTENT',               '207' => 'MULTI_STATUS',                    '208' => 'ALREADY_REPORTED',
    '226' => 'IM_USED',
    '300' => 'MULTIPLE_CHOICES',              '301' => 'MOVED_PERMANENTLY',               '302' => 'FOUND',
    '303' => 'SEE_OTHER',                     '304' => 'NOT_MODIFIED',                    '307' => 'TEMPORARY_REDIRECT',
    '308' => 'PERMANENT_REDIRECT',
    '400' => 'BAD_REQUEST',                   '401' => 'UNAUTHORIZED',                    '402' => 'PAYMENT_REQUIRED',
    '403' => 'FORBIDDEN',                     '404' => 'NOT_FOUND',                       '405' => 'METHOD_NOT_ALLOWED',
    '406' => 'NOT_ACCEPTABLE',                '407' => 'PROXY_AUTHENTICATION_REQUIRED',   '408' => 'REQUEST_TIMEOUT',
    '409' => 'CONFLICT',                      '410' => 'GONE',                            '411' => 'LENGTH_REQUIRED',
    '412' => 'PRECONDITION_FAILED',           '413' => 'PAYLOAD_TOO_LARGE',               '414' => 'URI_TOO_LONG URI',
    '415' => 'UNSUPPORTED_MEDIA_TYPE',        '416' => 'REQUESTED_RANGE_NOT_SATISFIABLE', '417' => 'EXPECTATION_FAILED',
    '418' => 'I_AM_A_TEAPOT',                 '422' => 'UNPROCESSABLE_ENTITY',            '423' => 'LOCKED',
    '424' => 'FAILED_DEPENDENCY',             '426' => 'UPGRADE_REQUIRED',                '428' => 'PRECONDITION_REQUIRED',
    '429' => 'TOO_MANY_REQUESTS',             '431' => 'REQUEST_HEADER_FIELDS_TOO_LARGE', '451' => 'UNAVAILABLE_FOR_LEGAL_REASONS',
    '500' => 'INTERNAL_SERVER_ERROR',         '501' => 'NOT_IMPLEMENTED',                 '502' => 'BAD_GATEWAY',
    '503' => 'SERVICE_UNAVAILABLE',           '504' => 'GATEWAY_TIMEOUT',                 '505' => 'HTTP_VERSION_NOT_SUPPORTED',
    '506' => 'VARIANT_ALSO_NEGOTIATES',       '507' => 'INSUFFICIENT_STORAGE',            '508' => 'LOOP_DETECTED',
    '509' => 'BANDWIDTH_LIMIT_EXCEEDED',      '510' => 'NOT_EXTENDED',                    '511' => 'NETWORK_AUTHENTICATION_REQUIRED',
};

# ------------------------------------------------------------------------------------ #
# file interface                                                                       #
# ------------------------------------------------------------------------------------ #

package FileInterface;

use Digest::MD5 qw(md5_base64);
use Encode qw(encode_utf8);
use Encode::Locale;
use File::Path qw(make_path);
use File::Spec;
use HTTP::Date		();
use HTTP::Request;
use HTTP::Response;
use JSON::PP;
use LWP::UserAgent	();
use LWP::MediaTypes qw(guess_media_type media_suffix);
use URI				();
use YAML::XS;

sub readFile
{
    my ($infile, $options) = @_;

    $options           = {} if !defined $options;

    my $encodingoption = "";
    $encodingoption    = ":encoding(UTF-8)" if exists $options->{u};

    my $infh;
    open($infh, "<${encodingoption}", $infile) || die("can't open \"$infile\": $!\n") if $infile ne "-";

    $infh = \*STDIN if $infile eq "-";

    my $file_text = do
    {
        local $/;
        <$infh>;
    };
    close($infh) if $infile ne "-";

    return $file_text;
}

sub readJSON
{
    my ($file, $options) = @_;

    $options      = {} if !defined $options;

    my $json_text = readFile($file, $options);

    my $output;

    eval {$output = decode_json(utf8::is_utf8($json_text) ? encode_utf8($json_text) : $json_text)};

    if (my $error = $@)
    {
        die "invalid json in $file: $error";
    }

    return $output;
}

sub readYAML
{
    my ($file, $options) = @_;

    $options      = {} if !defined $options;

    my $yaml_text = readFile($file, $options);

    my $output;

    # YAML::XS wants encoded strings. 'use utf8' automatically decodes strings inside of the script,
    # so we have to encode it again first to suppress errors :
    # wide character in Load
    eval {$output = Load(utf8::is_utf8($yaml_text) ? encode_utf8($yaml_text) : $yaml_text)};

    if (my $error = $@)
    {
        die "invalid yaml in $file: $error";
    }

    return $output;
}

sub makeDirectory
{
    my ($dirname) = @_;
    #	printf "making directory : %s\n", $dirname;
    make_path("$dirname");
    die "could not mkdir $dirname" if !-d "$dirname";
    return 0;
}

sub writeFile
{
    my ($outputpath, $filename, $recoded_text, $options) = @_;

    $options           = {} if !defined $options;

    my $ofile          = $filename;
    $ofile             = "${outputpath}/${ofile}" if $ofile ne "-";

    die "output file $ofile already exists" if $filename ne "-" && -f $ofile && !exists $options->{overwrite};

    my $encodingoption = "";
    $encodingoption    = ":encoding(UTF-8)" if exists $options->{u};

    my $outfh;
    open($outfh, ">${encodingoption}", $ofile) || die "Can't open \$ofile\": $!\n" if $ofile ne "-";

    $outfh = \*STDOUT if $ofile eq "-";

    $outfh->write($recoded_text);
    close($outfh) if $ofile ne "-";
}

sub findFormat
{
    my ($filename) = @_;

    return 'json' if $filename =~ m/\.json/;
    return 'yaml' if $filename =~ m/(\.yml)|(\.yaml)/;
    return 'undefined';
}

sub areFilesIdentical
{
    my ($classtext, $filename) = @_;

    my $existingclass = readFile($filename, {});

    # create a canonic representation of the
    # class source code for the 2 files and
    # compare the md5 checksums for them
    $classtext        =~ s/^\/\/\ From\ File\ :.*//g;
    $classtext        =~ s/^\n//;
    $classtext        =~ s/\r//g;
    $classtext        =~ s/\n/ /g;

    $existingclass    =~ s/^\/\/\ From\ File\ :.*//g;
    $existingclass    =~ s/^\n//;
    $existingclass    =~ s/\r//g;
    $existingclass    =~ s/\n/ /g;

    my $digestnew     = md5_base64(utf8::is_utf8($classtext)     ? encode_utf8($classtext)     : $classtext);
    my $digestold     = md5_base64(utf8::is_utf8($existingclass) ? encode_utf8($existingclass) : $existingclass);

    my $digestsMatch  = $digestnew eq $digestold;

    if (!$digestsMatch)
    {
        if ($classtext eq $existingclass)
        {
            warn sprintf "Digest mismatch while class texts identical: %s", $filename;
        }
    }

    return $digestsMatch;
}

##
#   writeModelClass : write a model class to an output file.
#                     create a new version if it is not identical to
#                     an already existing file under the same name.
##
sub writeModelClass
{
    my ($outputpath, $classname, $classtext) = @_;

    if ($outputpath eq '-')
    {
        print $classtext;
        return;
    }

    my $finished       = 0;
    my $mustWrite      = 1;
    my $collisionFound = 0;

    my $filename       = "${classname}.java";
    my $ofile          = "$outputpath/${filename}";
    my $version        = 0;

    while (-f $ofile && !$finished)
    {
        if (!areFilesIdentical($classtext, $ofile))
        {
            ++$version;
            $filename       = "${classname}-v${version}.java";
            $ofile          = "$outputpath/${filename}";
            $collisionFound = 1;
            $mustWrite      = 1;
        }
        else
        {
            $collisionFound = 0;
            $mustWrite      = 0;
            $finished       = 1;
        }
    }

    if ($mustWrite)
    {
        if ($collisionFound)
        {
            # annotate the file with last input file
            # that specified this version.
            printf "WARNING : writing new version ${filename}\n";
            if (exists $configoptions->{inputfile})
            {
                my $filename = $configoptions->{inputfile};
                $classtext = "// From\ File : $filename\n" . $classtext;
            }
        }

        printf "output : %s\n", $ofile if MAIN::checkOption($configoptions, 'verbose', 'flags');
        writeFile($outputpath, $filename, $classtext, {});
    }
}

##
#   readLocalFile:
#
#   read a file from a local file system.
#
#   Constraints :
#
#   - reject absolute paths
#   - reject relative paths that navigate out of the swagger directory
##
sub readLocalFile
{
    my ( $filename ) = @_;

    ##
    #   strip leading '../', './' or '/'
    #   TODO : configure location of external files
    ##
    $filename =~ s/^\.\///;     # strip './' since we make our own relative path
    $filename =~ s/^\///;       # strip '/' since we don't support absolute paths

    my $basepath = './';
    if (exists $configoptions->{inputfile})
    {
        my $parentFile  = $configoptions->{inputfile};
        $basepath       = dirname($parentFile);
        my $upwardSteps = () = $filename =~ /\.\.\//;
        die "[$parentFile] input file: ${filename}. navigating out of swagger file directory not supported"  if (scalar split '/', $basepath) < $upwardSteps;
    }

    $filename = "${basepath}/${filename}";

    my $inputformat = FileInterface::findFormat($filename);
    die "inputformat undefined"                 if !defined $inputformat;
    die "unsupported inputformat $inputformat"  if !exists $readInput->{$inputformat};

    my $externalhash        = $readInput->{$inputformat}->($filename);

    my $abs_filename        = realpath($filename);
    my $temp                = File::RelDir->New(realpath($basepath));
    my $relative_filename   = $temp->Path($abs_filename);

    if (!exists $mergedfiles->{$relative_filename})
    {
        printf "merge  : %s\n", $relative_filename if MAIN::checkOption($configoptions, 'verbose', 'flags');
        $mergedfiles->{$relative_filename} = '1';
    }

    return $externalhash;
}

##
#   downloadFile:
#
#   download a external reference from the remote url.
#   convert the response content to a perl hash table and return that to the caller.
##
sub downloadFile
{
    my ( $remoteUrl, $opt ) = @_;

    my $url         = URI->new( decode( locale => $remoteUrl ) );

    # configure a UserAgent object

    my $ua = LWP::UserAgent->new(
        agent      => "swaggercodegen/1.0",
        keep_alive => 1,
        env_proxy  => 1,
    );

    my $user        = $opt->{u}                                 if exists $opt->{u};
    my $pass        = $opt->{p}                                 if exists $opt->{p};

    $ua->credentials( $remoteUrl, "some realm", $user, $pass )  if defined $user && defined $pass;
    $ua->proxy( ['HTTP', 'HTTPS', 'FTP'], $opt->{x} )           if exists $opt->{x};
    $ENV{'http_proxy'}	= $opt->{x}					            if exists $opt->{x};
    $ENV{'https_proxy'}	= $opt->{x}					            if exists $opt->{x};

    my $response;
    my $req;

    # inquire the size of the file to download

    $req = HTTP::Request->new( HEAD => $url );
    $req->authorization_basic($user, $pass)		                if defined $user && defined $pass;
    $response = $ua->request( $req ) ;

    if (!$response->is_success)
    {
        die "retrieving information for URL=$url failed with HTTP error:". $response->status_line . "\n";
    }

    my $length	= $response->content_length;
    if ($length > 16*1024*1024)
    {
        die "length $length of download file exceeds 16MB limit";
    }

    # download the file

    $req = HTTP::Request->new( GET => $url );
    $req->authorization_basic($user, $pass)		                if defined $user && defined $pass;

    $response = $ua->request( $req );

    if (!$response->is_success)
    {
        die "download  failed: ".$response->status_line."\n";
    }

    if ( $response->header("X-Died"))
    {
        if ( my $died = $response->header("X-Died") )
        {
            print "$died\n";
        }
    }

    my $file_text    = $response->content();
    my $content_type = $response->header('content-type');

    my $inputformat;
    if (defined $content_type)
    {
        $inputformat = 'json' if $content_type =~ /json/i;
        $inputformat = 'yaml' if $content_type =~ /yaml/i;
    }

    $inputformat  = FileInterface::findFormat($remoteUrl) if !defined $inputformat;
    my $output;

    SWITCH:
    {
        ($inputformat =~ /json/i) && do {
            eval {$output = decode_json(utf8::is_utf8($file_text) ? encode_utf8($file_text) : $file_text)};

            if (my $error = $@)
            {
                die "invalid json : $error";
            }
            last SWITCH;
        };
        ($inputformat =~ /yaml/i) && do {
            eval {$output = Load(utf8::is_utf8($file_text) ? encode_utf8($file_text) : $file_text)};

            if (my $error = $@)
            {
                die "invalid yaml : $error";
            }
            last SWITCH;
        };
        do {
            die "unsupported input format $inputformat";
        };
    }

    return $output;
}

########################################################################
##
#   Dump the 'definitions' from the inputhash
##
########################################################################

package SwaggerUtils;

use Cwd qw(realpath);
use File::Basename;
use File::RelDir;

##
#   resolve a local reference to the input hash table
##
sub resolveLocalReference
{
    my ($ref) = @_;

    my @plist = split '[/]', $ref;

    my $datatype = $inputhash;

    for my $p (@plist)
    {
        die "$p from schema \$ref not found in \$inputhash" if !exists $datatype->{$p};
        $datatype = $datatype->{$p};
    }

    # the loop doesn't retain the last array element in the control variable,
    # so we have to explicitly pop the datatype name.

    ##
    #   CAVEAT: we enforce class names to begin with an uppercase letter
    ##

    my $name = pop @plist;
    $name    = ucfirst Parser::formatCamelCase($name, "[^A-Za-z0-9]+");

    if (ref $datatype ne 'HASH')
    {
        my $filename = 'swagger file';
        $filename = basename($configoptions->{inputfile}) if exists $configoptions->{inputfile};

        die "[$filename] $name : datatype is not a hash table";
    }

    my $result  = { name => $name, type => $datatype };

    ##
    #   tag the lookup result from the input hash if we already have a symbol table entry
    ##
    $result->{isSymbolDefined} = 'true' if exists $symboltable->{$result->{name}};

    return $result;
}

##
#   resolve a reference to a definition from an external file.
#   merge the external file into the input hash.
#   return a schema reference if the there is exactly one definition
#   in the file, else undef.
##
sub mergeExternalFile
{
    my ( $filename ) = @_;

    my $externalhash;

    if ( $filename =~ /^http[s]*:\/\/.*$/ )
    {
        $externalhash = FileInterface::downloadFile($filename);
    }
    else
    {
        $externalhash = FileInterface::readLocalFile($filename);
    }

    ##
    #   CAVEAT: copy ALL definitions from the external file to the input hash,
    #           so that we can have nested definitions as well.
    #
    #   Tag all copied definitions with the filename of origin.
    ##

    my $sectionnames    = qw/definitions|components|links|parameters/;

    my @sectionlist     = grep /$sectionnames/, keys %$externalhash;
    my $extref          = undef;
    my $definitionCount = 0;

    if (scalar @sectionlist == 0)
    {
        ##
        #   pick the directory name containing the file as the default section
        #   if it is one of the defined section names.
        ##
        my @filenamecomponents = split '/', $filename;
        pop @filenamecomponents;
        my $defaultsection     = pop @filenamecomponents;
        $defaultsection        = 'definitions' if $defaultsection !~ /$sectionnames/;

        ##
        #   if the external definitions come without a section, they go to the $defaultsection
        ##
        my $alreadydefined = 0;
        for my $key (keys %$externalhash)
        {
            $alreadydefined = 1 if exists $inputhash->{$defaultsection}{$key}{filename};

            if ( !$alreadydefined )
            {
                $externalhash->{$key}{filename} = $filename;
                $inputhash->{$defaultsection}{$key} = $externalhash->{$key};
            }
            else
            {
                if ( exists $inputhash->{$defaultsection}{$key}{filename} )
                {
                    my $previousFilename = $inputhash->{$defaultsection}{$key}{filename};
                    die "external reference $key resolves to 2 different files : $previousFilename - $filename" if $previousFilename ne $filename;
                }
            }

            $extref = "/$defaultsection/${key}";
            ++$definitionCount;
        }
    }
    else
    {
        for my $section (@sectionlist)
        {
            my $alreadydefined = 0;

            for my $key (keys %{$externalhash->{$section}})
            {
                $alreadydefined = 1 if exists $inputhash->{$section}{$key}{filename};

                if (!$alreadydefined)
                {
                    $externalhash->{$section}{$key}{filename} = $filename;
                }
                else
                {
                    if ( exists $inputhash->{$section}{$key}{filename} )
                    {
                        my $previousFilename = $inputhash->{$section}{$key}{filename};
                        die sprintf "external reference %s resolves to 2 different files : %s - %s", $key, $previousFilename, $filename if $previousFilename ne $filename;
                    }
                }

                $extref = "/${section}/$key";
                ++$definitionCount;
            }

            $inputhash->{$section} = $externalhash->{$section} if !$alreadydefined;
        }
    }

    ##
    #   return 'undef' if there is more than one definition in the file
    ##
    $extref = undef if $definitionCount > 1;

    return $extref;
}

##
#   resolveDataTypeReference :
#
#   resolve a reference to a composite data type
#   to an inputhash entry or a reference to a
#   primitive datatype.
#
#   import definitions from an external file, if the
#   reference is to a local or remote file.
##
sub resolveDataTypeReference
{
    my ($schema) = @_;

    die "\$schema is not a hash" if ref $schema ne 'HASH';

    if (!exists $schema->{'$ref'})
    {
        die "\$schema is missing \$ref key";
    }

    my $result;

    # split the reference path into components, then navigate the swagger hash table.
    # abort if we don't find the referred entry.

    my $ref = $schema->{'$ref'};
    my @fileandref = split '#', $ref;

    SWITCH:
    {
        ##
        #   process local references
        ##
        ( $ref =~ /^#/ ) && do
        {
            $ref    =~ s/^#\///;
            $result = resolveLocalReference($ref);
            last SWITCH;
        };
        ##
        #   process external references
        ##
        ( scalar @fileandref > 0) && do
        {
            my $parentfilename = 'swagger file';
            $parentfilename = basename($configoptions->{inputfile}) if exists $configoptions->{inputfile};

            die "[$parentfilename] external reference format is not '<file>#<ref> : $ref" if scalar @fileandref > 2;

            my $filename = $fileandref[0];
            my $extref   = mergeExternalFile($filename);

            if ( scalar @fileandref > 1 )
            {
                if (defined $extref && $extref ne $fileandref[1])
                {
                    die sprintf "[%s] reference %s from file not equal to declared reference %s", $filename, $extref, $fileandref[1];
                }

                $extref = $fileandref[1] if !defined $extref;
            }

            die "[$filename] no unique reference specified" if !defined $extref;

            $extref =~ s/^\///;

            ##
            #   resolve the reference locally after importing the definitions from the external file
            ##
            $result = resolveLocalReference( $extref );

            last SWITCH;
        };
        do
        {
            die "unsupported '\$ref' flavour : $ref";
        }
    }

    return $result;
}

sub padRight
{
    my ($s, $maxlen) = @_;
    $s .= ' ' x ($maxlen - length($s)) if length($s) < $maxlen;
    return $s;
}

sub definitionPrefix
{
    my ($type, $prefix) = @_;

    my $name;
    $prefix     = '' if !defined $prefix;
    my $derived = undef;

    SWITCH:
    {
        (scalar keys %$type == 0) && do
        {
            $name       = 'empty schema';
            $derived    = 'true';
            last SWITCH;
        };
        (exists $type->{'$ref'}) && do
        {
            $prefix    .= 'F';
            $name       = $type->{'$ref'};
            last SWITCH;
        };
        (ref Parser::extractPolymorphType($type) eq 'ARRAY') && do
        {
            my $subtype = Parser::extractPolymorphType($type);
            $prefix    .= 'Y';
            $name       = 'polymorph : ' . @{$subtype}[0];
            $derived    = 'true';
            last SWITCH;
        };
        (exists $type->{type} && ref $type->{type} eq 'ARRAY') && do
        {
            my $temp    = $type->{type};
            $temp       = Parser::extractNonNullElement($temp);

            if (exists $inbuilttypes->{$temp})
            {
                $name   = SymbolTable::resolveDatatypeName({ type => $temp });
            }
            elsif ($temp eq 'array')
            {
                my $result = definitionPrefix({ type => 'array', items => $type->{items} });
                $name      = $result->{name};
                $prefix   .= $result->{prefix};
            }
            else
            {
                die "unexpected subtype $temp";
            }
            last SWITCH;
        };
        (exists $type->{type} && $type->{type} eq 'array') && do
        {
            $type    = $type->{items} if exists $type->{items};
            $prefix .= 'A';

            if (exists $type->{'$ref'})
            {
                $name = $type->{'$ref'};
            }
            else
            {
                $name = SymbolTable::resolveDatatypeName($type);
            }
            last SWITCH;
        };
        (exists $type->{type} && exists $inbuilttypes->{$type->{type}}) && do
        {
            $name      = SymbolTable::resolveDatatypeName($type);
            last SWITCH;
        };
        (exists $type->{schema}) && do
        {
            $name      = 'schema';
            $derived   = 'true';
            last SWITCH;
        };
        (exists $type->{properties}) && do
        {
            #$prefix .= 'P';
            # my $pl = $type->{properties};
            # $name = $pl->[0]->{type }if ref $pl eq 'ARRAY' && scalar @$pl > 0;
            last SWITCH;
        };
        (exists $type->{parameters}) && do
        {
            #$prefix .= 'P';
            # my $pl = $type->{properties};
            # $name = $pl->[0]->{type }if ref $pl eq 'ARRAY' && scalar @$pl > 0;
            last SWITCH;
        };
        (exists $type->{additionalProperties}) && do
        {
            my $result = definitionPrefix($type->{additionalProperties}, "${prefix}N");
            $prefix    = $result->{prefix} if exists $result->{prefix};
            $name      = $result->{name}   if exists $result->{name};
            last SWITCH;
        };
        (exists $type->{type} && $type->{type} eq 'object') && do
        {
            $name      = 'object';
            $derived   = 'true';
            last SWITCH;
        };
        do
        {
            die "can't resolve type $type->{name}";
        };
    }

    $prefix    = padRight($prefix, 4);
    my $result = { prefix => $prefix, name => $name };
    $result->{derived} = $derived if defined $derived;
    return $result;
}

##
#   definitionToString :
#
#   resolve a type reference to a string.
##
sub definitionToString
{
    my ($type, $prefix) = @_;

    my $result = definitionPrefix($type, $prefix);

    $prefix    = '';
    $prefix    = $result->{prefix} if exists $result->{prefix} && defined $result->{prefix};

    my $name   = '';
    $name      = $result->{name} if exists $result->{name} && defined $result->{name};

    return padRight($prefix, 4) . " " . $name;
}

sub dumpPolymorphDefinition
{
    my ($level, $entry, $subtype, $options) = @_;

    map {
        my $inheritancetype = $_;
        my $polymorph = $entry->{$inheritancetype};
        map {
            my $component = $_;
            dumpDefinition($level + 1, undef, $component, $options);
        } @$polymorph;
    } @$subtype;
}

##
#   dumpArray :
#   dump array elements
##
sub dumpArray
{
    my ($level, $tag, $list) = @_;

    my $indent = ' ' x (4 * $level);

    my $index  = 0;
    my $state  = 0;

    for my $item (@$list)
    {
        SWITCH:
        {
            (ref $item eq '') && do
            {
                printf "${indent}%-18s [%2d] : %s\n", $tag, $index, CodeGenerator::formatDescriptionAsOneLine($item, 100 - 4 * $level, { dontescapeapostrophes => 'true' });
                last SWITCH;
            };
            (ref $item eq 'HASH' && scalar keys %$item == 1) && do
            {
                my $key = (keys %$item)[0];
                my $val = $item->{$key};

                if (ref $val eq '')
                {
                    printf "${indent}%-18s [%2d] : %s\n", $tag, $index, CodeGenerator::formatDescriptionAsOneLine($val, 100 - 4 * $level, { dontescapeapostrophes => 'true' });
                }
                else
                {
                    dumpItem($level + 1, "${tag} :: ${key}", $val);
                }
                last SWITCH;
            };
            do
            {
                printf "${indent}%-18s\n", $tag if !$state;
                dumpItem($level + 1, (sprintf "[%2d]", $index), $item);
                $state = 1;
                last SWITCH;
            };
        }
        ++$index;
    }
}

##
#   dumpHash :
#   dump hash table elements
##
sub dumpHash
{
    my ($level, $tag, $hash) = @_;

    my $indent = ' ' x (4 * $level);

    printf "${indent}%-18s\n", $tag;
    for my $key (sort keys %$hash)
    {
        my $val = $hash->{$key};
        dumpItem($level + 1, $key, $val);
    }
}

sub dumpItem
{
    my ($level, $tag, $item) = @_;

    return if !defined $item;

    my $indent = ' ' x (4 * $level);

    SWITCH:
    {
        (ref $item eq 'ARRAY') && do
        {
            if (scalar @$item == 0)
            {
                printf "${indent}%-18s\n", $tag;
            }
            else
            {
                dumpArray($level, $tag, $item);
            }
            last SWITCH;
        };
        (ref $item eq 'HASH') && do
        {
            if (scalar keys %$item == 0)
            {
                printf "${indent}%-18s\n", $tag;
            }
            else
            {
                dumpHash($level, $tag, $item);
            }
            last SWITCH;
        };
        do
        {
            printf "${indent}%-18s : %s\n", $tag, CodeGenerator::formatDescriptionAsOneLine($item, 100 - 4 * $level, { dontescapeapostrophes => 'true' });
            last SWITCH;
        };
    }
}

##
#   dumpDefinition :
#
#   dump a single definition entry
##
sub dumpDefinition
{
    my ($level, $entryname, $entry, $options) = @_;

    # if ( $entryname eq 'ContainerConfig')
    # {
    #     printf "found!\n";
    # }

    my $indent    = ' ' x (4 * $level);

    die "\$options must be a hash" if ref $options ne 'HASH';

    my $itemlist  = [];
    $itemlist     = $options->{parentlist} if exists $options->{parentlist};

    my $polymorph = Parser::extractPolymorphType($entry);

    my $prefix    = "";
    $prefix       = $options->{type} if exists $options->{type};

    SWITCH:
    {
        (scalar keys %$entry == 0) && do
        {
            my $prefix = "    ";
            $prefix    = $options->{type} . "   " if exists $options->{type};
            push @$itemlist, { indent => $indent, tostring => $prefix . 'empty scheme' };
            last SWITCH;
        };
        (scalar keys %$entry == 1 && exists $entry->{examples}) && do
        {
            my $prefix = "    ";
            $prefix    = $options->{type} . "   " if exists $options->{type};
            push @$itemlist, { indent => $indent, tostring => $prefix . 'empty scheme' };
            last SWITCH;
        };
        (exists $entry->{schema}) && do
        {
            ##
            # push an empty line above a parameter property list
            # to prevent the parameter name from swallowing the 1st list item
            ##
            push @$itemlist, { indent => $indent, tostring => 'scheme' } if $level == 0 && !exists $options->{definition};
            dumpDefinition($level + 1, undef, $entry->{schema}, { parentlist => $itemlist, type => "${prefix}S" });
            last SWITCH;
        };
        (exists $entry->{content}) && do
        {
            my $content = $entry->{content};
            for my $contenttype (sort keys %$content)
            {
                my $ref = $content->{$contenttype};
                dumpDefinition($level + 1, undef, $ref, { parentlist => $itemlist, type => "${prefix}T" });
            }
            last SWITCH;
        };
        (exists $entry->{links}) && do
        {
            my $links = $entry->{links};

            die "'links' must be a hash ref" if ref $links ne 'HASH';

            for my $ref (keys %$links)
            {
                dumpDefinition($level + 1, $ref, $links->{$ref}, { parentlist => $itemlist, type => "${prefix}L" });
            }

            last SWITCH;
        };
        (exists $entry->{components}) && do
        {
            dumpDefinition($level + 1, undef, $entry->{components}, { parentlist => $itemlist, type => "${prefix}C" });
            last SWITCH;
        };
        (exists $entry->{'$ref'}) && do
        {
            push @$itemlist, { indent => $indent, tostring => definitionToString($entry, $prefix) };
            last SWITCH;
        };
        (exists $entry->{properties}) && do
        {
            my $required = undef;

            if (exists $entry->{required})
            {
                $required = {};
                map {$required->{$_} = 1;} @{$entry->{required}};
            }

            my $properties = $entry->{properties};

            for my $name (sort keys %$properties)
            {
                my $type = $properties->{$name};

                my $property = { indent => $indent, tostring => definitionToString($type, "${prefix}P"), name => $name };

                if (defined $required)
                {
                    my $requiredtag = "optional";
                    $requiredtag = "mandatory" if exists $required->{$name};
                    $property->{required} = $requiredtag;
                }

                $property->{description} = $type->{description} if exists $type->{description};

                push @$itemlist, $property;

                if (exists $type->{type} && $type->{type} eq 'object' && exists $type->{properties})
                {
                    dumpDefinition($level + 1, undef, $type, { parentlist => $itemlist, type => $prefix });
                }
            }

            last SWITCH;
        };
        (exists $entry->{parameters}) && do
        {
            my $parameters = $entry->{parameters};

            for my $name (sort keys %$parameters)
            {
                my $type        = $parameters->{$name};
                my $localprefix = "${prefix}R";
                my $parameter   = { indent => $indent, tostring => padRight($localprefix, 4) . " " . $type, name => $name };

                push @$itemlist, $parameter;
            }

            last SWITCH;
        };
        (exists $entry->{additionalProperties}) && do
        {
            my $additionalProperties = $entry->{additionalProperties};
            dumpDefinition($level + 1, undef, $additionalProperties, { parentlist => $itemlist, type => "${prefix}N" });
            last SWITCH;
        };
        (exists $entry->{type} && $entry->{type} eq 'array') && do
        {
            if (exists $entry->{items})
            {
                dumpDefinition($level + 1, undef, $entry->{items}, { parentlist => $itemlist, type => "${prefix}A" });
            }
            last SWITCH;
        };
        (defined $polymorph && scalar @$polymorph > 0) && do
        {
            dumpPolymorphDefinition($level + 1, $entry, $polymorph, { parentlist => $itemlist, type => "${prefix}Y" });
            last SWITCH;
        };
        (exists $entry->{type} && $entry->{type} eq 'object') && do
        {
            push @$itemlist, { indent => $indent, tostring => definitionToString($entry, "${prefix}O") };
            last SWITCH;
        };
        (exists $entry->{type} && ref $entry->{type} eq "") && do
        {
            push @$itemlist, { indent => $indent, tostring => padRight("${prefix}D", 4) . $entry->{type} };
            last SWITCH;
        };
        do
        {
            die "unsupported type definition";
        };
    }

    if ($level == 0)
    {
        my $description;
        $description = $options->{description} if exists $options->{description};
        $description = $entry->{description} if exists $entry->{description};
        $description = $entry->{summary} if exists $entry->{summary} && $description eq "";

        $description = CodeGenerator::formatDescriptionAsOneLine($description, 72);
        $description = "" if !defined $description;

        my $type     = "";
        $type        = $entry->{type} if exists $entry->{type};

        my $subtype  = "";

        if (defined $polymorph && scalar @$polymorph > 0)
        {
            $subtype = @{$polymorph}[0];
            $type    = 'polymorph';
        }

        shift @$itemlist if (scalar @$itemlist == 2 && $itemlist->[0]->{tostring} eq 'scheme');

        SWITCH:
        {
            (exists $options->{param}) && do
            {
                my $required = CodeGenerator::extractParamCardinality($entry, [ 'mandatory', 'optional ' ]);
                my $param    = shift @$itemlist;

                printf "[%-8s] %-60s : [%-9s] : %-20s : %s\n",
                    $options->{in}
                    , $param->{tostring}
                    , $required
                    , $options->{param}
                    , $description;

                last SWITCH;
            };
            (exists $options->{response}) && do
            {
                my $response = shift @$itemlist;

                my $name     = "";
                $name        = $response->{name} if exists $response->{name};

                printf "[%-8s] %-60s    %-9s  : %-20s : %s\n"
                    , $options->{response}
                    , $response->{tostring}
                    , " "
                    , $name
                    , $description;

                last SWITCH;
            };
            (exists $options->{definition}) && do
            {
                my $p    = definitionPrefix($entry);
                my $info = "";
                $info    = $p->{name} if defined $p->{derived};
                printf "%-s %-72s [%-20s] : %s\n", $p->{prefix}, $options->{definition}, $info, $description;
                last SWITCH;
            };
        }

        for my $item (@$itemlist)
        {
            my $name     = "";
            $name        = $item->{name} if exists $item->{name};
            my $description;
            $description = $item->{description} if exists $item->{description};
            $description = CodeGenerator::formatDescriptionAsOneLine($description, 72);
            $description = "" if !defined $description;
            if (exists $item->{required})
            {
                printf "%s%-72s [%-9s] : %-30s : %s\n", $item->{indent}, $item->{tostring}, $item->{required}, $name, $description;
            }
            else
            {
                printf "%s%-72s : %-30s : %s\n", $item->{indent}, $item->{tostring}, $name, $description;
            }
        }
    }
}

##
#   dumpDefinitionsTable :
#
#   dump all definitions from the input hash
##
sub dumpDefinitionsTable
{
    my $filename = 'swagger file';
    $filename = $configoptions->{inputfile} if exists $configoptions->{inputfile};

    printf "%s\n", '=' x 132;
    printf "Definitions from : %s\n", basename($filename);
    printf "%s\n", '-' x 132;

    if (exists $inputhash->{definitions})
    {
        printf "Definitions\n";
        printf "%s\n", '-' x 132;

        my $definitions = $inputhash->{definitions};

        for my $entryname (sort keys %$definitions)
        {
            my $entry = $definitions->{$entryname};
            dumpDefinition(0, $entryname, $entry, { definition => $entryname });
            printf "%s\n", '-' x 132;
        }
    }

    if (exists $inputhash->{components})
    {
        printf "Components\n";
        printf "%s\n", '-' x 132;

        my $components = $inputhash->{components};

        for my $section (sort keys %$components)
        {
            for my $entryname (sort keys %$section)
            {
                my $entry = $section->{$entryname};
                dumpDefinition(0, $entryname, $entry, { definition => $entryname, type => uc substr($section, 0, 1) });
                printf "%s\n", '-' x 132;
            }
        }
    }

    if (exists $inputhash->{parameters})
    {
        printf "Parameters\n";
        printf "%s\n", '-' x 132;

        my $parameters = $inputhash->{parameters};

        for my $paramkey (sort keys %$parameters)
        {
            my $param = $parameters->{$paramkey};

            dumpDefinition(0, $paramkey, $param, { param => $param->{name}, in => $param->{in} });
            printf "%s\n", '-' x 132;
        }
    }
}

########################################################################
##
#   Dump the endpoints
##
########################################################################

sub dumpEndPointParameters
{
    my ($endpoint, $method, $parameters) = @_;

    printf("%s\n", "Parameters:");
    printf("%s\n", "==========");

    for my $param (@$parameters)
    {
        my $isIndirect = 0;

        if (!exists $param->{in})
        {
            die "reference expected while missing {in} : ${method}::${endpoint}" if !exists $param->{'$ref'};
            $param      = resolveDataTypeReference($param);
            $param      = $param->{type};
            $isIndirect = 1;
        }

        my $name        = "<undefined>";
        $name           = CodeGenerator::formatDescriptionAsOneLine($param->{name},        25, { dontescapeapostrophes => 'true' }) if exists $param->{name};
        my $description = CodeGenerator::formatDescriptionAsOneLine($param->{description}, 72, { dontescapeapostrophes => 'true' }) if exists $param->{description};
        $description    = CodeGenerator::formatDescriptionAsOneLine($param->{summary},     72, { dontescapeapostrophes => 'true' }) if exists $param->{summary} && $description eq "";
        $description    = "" if !defined $description;

        my $options = {
            descriptions => $description,
            param        => $name,
            in           => $param->{in}
        };
        $options->{type} = 'I' if $isIndirect;

        SWITCH:
        {
            ($param->{in} eq 'body') && do {
                dumpDefinition(0, undef, $param, $options);
                last SWITCH;
            };
            ($param->{in} =~ m/header|path|query|formData/) && do {
                dumpDefinition(0, undef, $param, $options);
                last SWITCH;
            };
            do {
                die "$param->{name} : unsupported parameter type $param->{in}";
            };
        }
    }
}

sub dumpEndPointResponses
{
    my ($endpoint, $method, $responses) = @_;

    printf("%s\n", "Responses :");
    printf("%s\n", "=========  ");

    for my $key (sort keys %$responses)
    {
        my $response   = $responses->{$key};

        my $typePrefix = "";
        my $typeName   = "";
        if (MAIN::checkOption($configoptions, 'useErrorResponse') && $key !~ /^[12]/)
        {
            $typePrefix = 'ErrorResponse';
            $typeName   = "";
        }
        else
        {
            $typePrefix = Parser::generateTypePrefix("${endpoint}-${method}");
            $typeName   = "Rc${key}";
        }

        my $description = CodeGenerator::formatDescriptionAsOneLine($response->{description}, 72, { dontescapeapostrophes => 'true' }) if exists $response->{description};
        $description    = "" if !defined $description;

        ##
        #   process all variants of response layout
        ##
        for my $responsetype (sort grep /schema|links|content/, keys %$response)
        {
            SWITCH:
            {
                (exists $response->{$responsetype}) && do
                {
                    dumpDefinition(0, undef, { $responsetype => $response->{$responsetype} },
                        {
                            descriptions => $description,
                            response     => $key
                        });
                    last SWITCH;
                };
                do
                {
                    printf "[%-8s] %-60s    %-9s  : %-20s : %s\n"
                        , $key
                        , "<no schema>"
                        , " "
                        , " "
                        , $description;
                }
            }
        }
    }
}

##
#   dumpEndPointOperation :
#
#   process a single operation for an endpoint.
##
sub dumpEndPointOperation
{
    my ($endpoint, $method) = @_;

    my $paths    = $inputhash->{paths};
    my $pi       = $paths->{$endpoint};
    my $mi       = $pi->{$method};

    my $description;
    $description = CodeGenerator::formatDescriptionAsOneLine($mi->{description}, 72, { dontescapeapostrophes => 'true' }) if exists $mi->{description};
    $description = CodeGenerator::formatDescriptionAsOneLine($mi->{summary},     72, { dontescapeapostrophes => 'true' }) if exists $mi->{summary};
    $description = "" if !defined $description;

    printf("[%-8s] %-72s : %s\n", $method, $endpoint, $description);

    for my $tag (sort grep !/parameters|responses/, keys %$mi)
    {
        my $item = $mi->{$tag};
        dumpItem(0, $tag, $item);
    }

    printf "%s\n", '-' x 132;

    if (exists $mi->{consumes})
    {
        my $consumes = $mi->{consumes};
        printf("consumes : %s\n", Parser::array2ConcatenatedStrings($consumes, { noapostrophes => 'true' }));
    }

    if (exists $mi->{produces})
    {
        my $produces = $mi->{produces};
        printf("produces : %s\n", Parser::array2ConcatenatedStrings($produces, { noapostrophes => 'true' }));
    }

    printf "%s\n", '-' x 132;

    ##
    #   process input parameters
    ##

    my $parameters = [];
    map {push @$parameters, $_} @{$pi->{parameters}} if exists $pi->{parameters};
    map {push @$parameters, $_} @{$mi->{parameters}} if exists $mi->{parameters};

    if (defined $parameters)
    {
        dumpEndPointParameters($endpoint, $method, $parameters);
    }

    ##
    #   process output response types
    ##
    dumpEndPointResponses($endpoint, $method, $mi->{responses});

    printf "%s\n", '-' x 132;
}

##
#   dumpEndPointInformation :
#
#   process a single endpoint.
##
sub dumpEndPointInformation
{
    my ($endpoint) = @_;

    if ($endpoint =~ m/:/)
    {
        my @s = split ':', $endpoint;

        $endpoint = $s[0];
        my $method = $s[1];

        dumpEndPointOperation($endpoint, $method);
    }
    else
    {
        for my $method (grep !/parameters/, sort keys %{$inputhash->{paths}{$endpoint}})
        {
            dumpEndPointOperation($endpoint, $method);
        }
    }
}

sub dumpSwaggerfile
{
    my ($endpoints) = @_;

    my $filename = 'swagger file';
    $filename    = $configoptions->{inputfile} if exists $configoptions->{inputfile};

    printf "%s\n", '=' x 132;
    printf "Contents of swagger file : %s\n", basename($filename);

    for my $tag (sort grep !/components|contents|definitions|links|parameters|paths/, keys %$inputhash)
    {
        my $item = $inputhash->{$tag};
        dumpItem(0, $tag, $item);
    }

    printf "%s\n", '=' x 132;
    printf "Endpoints from : %s\n", basename($filename);
    printf "%s\n", '-' x 132;

    for my $endpoint (sort keys %$endpoints)
    {
        dumpEndPointInformation($endpoint);
    }

    printf "\n";

    dumpDefinitionsTable();
}

########################################################################
##
#   Manage the symbol table
##
########################################################################

package SymbolTable;

##
#   dumpSymboltableEntry :
#
#   print a single symbol table entry
##
sub dumpSymboltableEntry
{
    my ($entryname) = @_;

    my $entry;

    die "$entryname is not a defined symbol" if !exists $symboltable->{$entryname};
    $entry      = $symboltable->{$entryname};

    # if ($entryname eq 'ContainerSummaryNetworkSettings')
    # {
    #     printf "found!\n";
    # }

    my $type    = "";
    my $subtype = "";

    if (exists $entry->{exposed})
    {
        $type .= sprintf ' %-8s', $entry->{exposed};
    }

    if (exists $entry->{composed})
    {
        $subtype = sprintf "polymorph: %s", $entry->{composed};
    }

    if (exists $entry->{extendedbasetype})
    {
        my $extendedbasetype = $entry->{extendedbasetype};
        $subtype = sprintf "extends %s", $extendedbasetype->{name};
    }

    my $description = CodeGenerator::formatDescriptionAsOneLine($entry->{description}, 72, { dontescapeapostrophes => 'true' }) if exists $entry->{description};
    $description    = "" if !defined $description;

    printf "%s\n", '-' x 132;
    printf "%-51s  %-11s : %-24s : %s\n", $entryname, $type, $subtype, $description;

    SWITCH:
    {
        (exists $entry->{definition}) && do
        {
            my $definition = $entry->{definition};

            my $modifier   = ' ' x 4;
            $modifier      = 'A   ' if exists $entry->{type} && $entry->{type} eq 'array';
            my $typename   = "undefined type";
            $typename      = $definition->{type} if exists $definition->{type};
            $typename      = "enum" if exists $definition->{enum};

            my $required   = "optional ";
            $required      = $entry->{required} if exists $entry->{required};

            printf "%-4s %-60s [%-9s] %-24s : %s\n"
                , $modifier
                , $typename
                , $required
                , "[alias]"
                , " ";

            last SWITCH;
        };
        (exists $entry->{reference}) && do
        {
            my $modifier = 'F   ';
            $modifier    = 'FA  ' if exists $entry->{type} && $entry->{type} eq 'array';
            my $typename = $entry->{reference};

            my $required = "optional ";
            $required    = $entry->{required} if exists $entry->{required};

            printf "%-4s %-60s [%-9s] %-24s : %s\n"
                , $modifier
                , $typename
                , $required
                , "[alias]"
                , " ";

            last SWITCH;
        };
        (exists $entry->{isMap}) && do
        {
            my $hashValType = $entry->{hashValType};
            my $modifier = 'M   ';
            $modifier    = 'MA  ' if exists $hashValType->{type} && $hashValType->{type} eq 'array';

            my $typename   = deriveMapTypeDeclaration($entry, {});

            my $required = "optional ";
            $required    = $entry->{required} if exists $entry->{required};

            printf "%-4s %-60s [%-9s] %-24s : %s\n"
                , $modifier
                , $typename
                , $required
                , "[map]"
                , " ";

            last SWITCH;
        };
        (exists $entry->{attriblist}) && do
        {
            for my $p (@{$entry->{attriblist}})
            {
                my $modifier = '';
                $modifier   .= 'A' if exists $p->{type} && $p->{type} eq 'array';
                $modifier   .= 'M' if exists $p->{isMap};
                $modifier   .= 'D' if exists $p->{isDummy} || exists $p->{isDummyClass};

                $modifier    = substr($modifier.(' 'x 4), 0, 4);

                my $typename = SymbolTable::resolveSymbolAttribTypeName($p);

                my $name     = "<undefined>";
                $name        = $p->{name} if exists $p->{name};

                my $required = "optional ";
                $required    = $p->{required} if exists $p->{required};

                my $description;
                $description = CodeGenerator::formatDescriptionAsOneLine($p->{description}, 72, { dontescapeapostrophes => 'true' }) if exists $p->{description};
                $description = "" if !defined $description;

                printf "%-4s %-60s [%-9s] %-24s : %s\n"
                    , $modifier
                    , $typename
                    , $required
                    , $name
                    , $description;
            }
            last SWITCH;
        };
        do {
            die "unsupport symbol type $entryname";
        }
    }
}

##
#   dumpSymboltable :
#
#   dump all symbol table entries to stdout.
##
sub dumpSymboltable
{
    my ($configoptions) = @_;

    printf "%s\n", '-' x 132;

    printf "=== Symbol Table Entries\n" if scalar keys %$symboltable > 0;

    for my $entryname (sort keys %$symboltable)
    {
        dumpSymboltableEntry($entryname, $configoptions);
    }

    printf "%s\n", '-' x 132;
}

########################################################################
##
#       Build the symbol table
#
#       go over all methods from the input hash that accept or produce
#       a data type and enter that into the symbol table if it is a
#       complex data type.
##
########################################################################

sub createSymboltableEntry
{
    my ($entry) = @_;

    if (ref $entry ne 'HASH')
    {
        die "\$entry is not a hash";
    }

    if (!exists $entry->{name})
    {
        die "\$entry is missing 'name'";
    }

    my $name = $entry->{name};

    # if ($name eq "IPAM")
    # {
    #     printf "found!\n";
    # }

    if ($name !~ /^[A-Z]/)
    {
        die "type name $name does not start with upper case letter";
    }

    my $filename = 'swagger file';
    $filename    = $configoptions->{inputfile} if exists $configoptions->{inputfile};

    ##
    #   handle symbol table collisions
    ##
    if (exists $symboltable->{$name} && $symboltable->{$name}{status} ne 'open')
    {
        my $sref = $symboltable->{$name};
        return createSymboltableReference($name) if exists $sref->{status} && $sref->{status} eq 'closed';
    }

    ##
    #   create an empty symbol table entry
    #   (perl apparently has a problem creating hash table entries if no empty hash table exists)
    ##
    $symboltable->{$name} = {};

    my $sref = $symboltable->{$name};

    map {
        $sref->{$_} = $entry->{$_};
    } keys %$entry;

    $sref->{filename} = $filename;
    $sref->{status}   = 'closed';

    return $sref;
}

sub createSymboltableReference
{
    my ($entryname) = @_;

    if (!exists $symboltable->{$entryname})
    {
        die "entry $entryname is missing";
    }

    my $result = { name => $entryname, reference => $entryname };

    return $result;
}

sub resolveSymboltableReference
{
    my ($reference) = @_;

    die "reference without reference"        if !exists $reference->{reference};
    my $name = $reference->{reference};
    die "symbol table entry $name not found" if !exists $symboltable->{$name};

    return $symboltable->{$name};
}

##
#   resolveDatatypeName :
#
#   resolve a type reference to a name of a symbol table entry
#   return 'undef' if no such entry exists
##
sub resolveDatatypeName
{
    my ($symbol) = @_;

    if (ref $symbol ne 'HASH')
    {
        die "\$symbol is not a hash";
    }

    my $name;
    SWITCH:
    {
        (exists $symbol->{isMap}) && do
        {
            #die sprintf "%s : can't resolve a map to a symbol table entry", $symbol->{name};
            $name = undef;
            last SWITCH;
        };
        (exists $symbol->{definition}) && do
        {
            die "can't resolve definition in $symbol->{name}"    if !exists $symbol->{definition}{type};
            $name = $symbol->{definition}{type};
            last SWITCH;
        };
        (exists $symbol->{reference}) && do
        {
            $name = $symbol->{reference};
            last SWITCH;
        };
        (exists $symbol->{name}) && do
        {
            $name = $symbol->{name};
            last SWITCH;
        };
        (exists $symbol->{type} && $symbol->{type} eq 'array') && do
        {
            die "'items' component missing in array declaration" if !exists $symbol->{items};
            $symbol = $symbol->{items};
            $name   = SymbolTable::resolveDatatypeName($symbol);
            last SWITCH;
        };
        (exists $symbol->{type} && exists $inbuilttypes->{$symbol->{type}}) && do
        {
            $name   = $symbol->{type};
            last SWITCH;
        };
        (exists $symbol->{type} && $symbol->{type} eq 'object') && do
        {
            $name   = 'object';
            $name   = $symbol->{name} if exists $symbol->{name};
            last SWITCH;
        };
        (scalar keys %$symbol == 0) && do
        {
            $name   = 'string';
            last SWITCH;
        };
        do
        {
            $name = "<undefined>";
            $name   = $symbol->{name} if exists $symbol->{name};
            die "can't resolve type $name";
        };
    }

    return $name;
}

##
#   deriveMapTypeDeclaration :
#
#   generate a map type declaration from key and value types
##
sub deriveMapTypeDeclaration
{
    my ($mapType, $classimports, $options ) = @_;

    my $keyType = $mapType->{hashKeyType};
    die "unsupported key type $keyType" if !exists $inbuilttypes->{$keyType};
    $keyType = $inbuilttypes->{$keyType};

    my $valType;

    my $temp = $mapType->{hashValType};

    SWITCH: {
        (exists $temp->{definition}) && do {
            $temp = $temp->{definition};
            die "hash value definition is missing 'type'" if !exists $temp->{type};
            $temp = $temp->{type};
            die "unsupported val type $temp" if !exists $inbuilttypes->{$temp};
            $valType = $inbuilttypes->{$temp};
            last SWITCH;
        };
        (exists $temp->{reference}) && do {
            $valType = $temp->{reference};
            $classimports->{$valType} = 1 if defined $classimports && ref $classimports eq 'HASH';
            last SWITCH;
        };
        do {
            die "unsupported Hash Value Type";
        };
    }

    my $idmap   = 'Map';
    my $idlist  = 'List';

    if ( defined $options && ref $options eq 'HASH' && exists $options->{usejava})
    {
        $idmap   = 'HashMap';
        $idlist  = 'ArrayList';
    }

    $temp = $mapType->{hashValType};
    if (exists $temp->{type} && $temp->{type} eq 'array')
    {
        $valType = "${idlist}<${valType}>";
    }

    return "${idmap}<${keyType},${valType}>";
}


##
#   resolveSymbolAttribTypeName :
#
#   extract the attribute type
##
sub resolveSymbolAttribTypeName
{
    my ($symbolattribute) = @_;

    my $typename;

    SWITCH:
    {
        (exists $symbolattribute->{definition}) && do
        {
            $typename = $symbolattribute->{definition}{type};
            last SWITCH;
        };
        (exists $symbolattribute->{reference}) && do
        {
            $typename = $symbolattribute->{reference};
            last SWITCH;
        };
        (exists $symbolattribute->{isMap}) && do
        {
            $typename = deriveMapTypeDeclaration($symbolattribute, {'usejava'=>'true'});
            last SWITCH;
        };
        do {
            my $name = $symbolattribute->{name} if exists $symbolattribute->{name};
            $name = "<undefined>" if !defined $name;
            die "can't resolve symbol attribute type of attribute $name";
        }
    }

    return $typename;
}

##
#   resolve symbol table aliases
##
sub resolveTypeAlias
{
    my ($attr) = @_;

    die "INTERNAL: attribute must be a hash reference" if ref $attr ne 'HASH';

    my $isArray = 0;
    $isArray    = 1 if exists $attr->{type} && $attr->{type} eq 'array';

    my $result  = { attribute => $attr };
    $result->{isArray} = $isArray            if $isArray;

    return $result if !exists $attr->{reference};

    while (exists $attr->{reference})
    {
        my $alias = $attr->{reference};
        die "resolveTypeAlias : symbol $alias not found" if !exists $symboltable->{$alias};
        $attr    = $symboltable->{$alias};
        $isArray = 1 if exists $attr->{type} && $attr->{type} eq 'array';
    }

    $result = { attribute => $attr, isDereferenced => 1 };
    $result->{isArray} = $isArray if $isArray;

    return $result;
}

##
#   findMismatches :
#
#   return the number of mismatches between 2 types
##
sub findMismatches
{
    my ($symbolname, $oldtype, $newtype) = @_;

    die "findMismatches: oldtype is not a hash for symbol $symbolname" if ref($oldtype) ne 'HASH';
    die "findMismatches: newtype is not a hash for symbol $symbolname" if ref($newtype) ne 'HASH';

    die "findMismatches: oldtype is missing 'attriblist"               if !exists $oldtype->{attriblist};
    die "findMismatches: newtype is missing 'attriblist"               if !exists $newtype->{attriblist};

    my $oldfilename = basename($oldtype->{filename});
    my $newfilename = basename($newtype->{filename});

    my $typetable = {};

    for my $p (@{$oldtype->{attriblist}})
    {
        my $name            = $p->{name};
        my $type            = SymbolTable::resolveSymbolAttribTypeName($p);
        my $required        = exists $p->{required} ? $p->{required} : "optional";
        $typetable->{$name} = { typename => $type, required => $required };
    }

    my $mismatches = 0;

    for my $p (@{$newtype->{attriblist}})
    {
        my $name     = $p->{name};
        my $typename = SymbolTable::resolveSymbolAttribTypeName($p);

        if (exists $typetable->{$name})
        {
            my $q = $typetable->{$name};
            if ($typename ne $q->{typename} || $p->{required} ne $q->{required})
            {
                printf "Mismatch [old:$oldfilename new:$newfilename] [$symbolname]\n" if !$mismatches;
                printf "[$name] types : $typename vs $q->{typename} required : $p->{required} vs $q->{required}\n";
                ++$mismatches;
            }
        }
        else
        {
            printf "Mismatch [old:$oldfilename new:$newfilename] [$symbolname]\n" if !$mismatches;
            printf "attribute $name missing in old\n";
            ++$mismatches;
        }

        $typetable->{$name}{verified} = 1;
    }

    for my $name (keys %$typetable)
    {
        if (!exists $typetable->{$name}{verified})
        {
            printf "Mismatch [old:$oldfilename new:$newfilename] [$symbolname]\n" if !$mismatches;
            printf "attribute $name missing in new\n";
            ++$mismatches;
        }
    }

    return $mismatches;
}

########################################################################
##
#       recursively descend into the subtypes of the argument type
##
########################################################################

package Parser;

use File::Basename;
use Storable qw(dclone);

sub array2ConcatenatedStrings
{
    my ($list, $options) = @_;

    die "\$list is not an array" if ref($list) ne 'ARRAY';

    return undef if scalar @$list == 0;

    my $additionalentry = $options->{additionalentry} if defined $options && ref $options eq 'HASH' && exists $options->{additionalentry};

    my $temp  = $list;
    if (defined $additionalentry)
    {
        $temp     = [];
        my $found = 0;
        for my $item (@$list)
        {
            push @$temp, $item;
            $found = 1 if $item =~ $additionalentry;
        }

        push @$temp, $additionalentry if !$found;
    }

    my $liststring = "";
    my $delim      = "";

    my $apostrophe = "\"";
    $apostrophe    = '' if defined $options && ref $options eq 'HASH' && exists $options->{noapostrophes};

    map
    {
        $liststring .= "${delim}${apostrophe}$_${apostrophe}";
        $delim = ", ";
    } @$temp;

    return $liststring
}

sub extractNonNullElement
{
    my ($itemlist) = @_;

    my $result = undef;
    for my $item (@$itemlist)
    {
        if ($item ne 'null')
        {
            die "more than 1 non-null element in itemlist" if defined $result;
            $result = $item;
        }
    }

    return $result;
}

##
#   extractPolymorphType :
#
#   extract one of the supported polymorph declarations
#   from the argument type.
##
sub extractPolymorphType
{
    my ($type) = @_;

    return undef if ref $type ne 'HASH';

    my $result      = [];
    my $isPolymorph = 0;

    map {
        push @$result, $_;
        $isPolymorph = 1;
    } grep /anyOf|allOf|oneOf/, keys %$type;

    return undef if !$isPolymorph;

    return $result;
}

##
#   formatCamelCase :
#
#   make each delimited substring camelcase-compliant
##
sub formatCamelCase
{
    my ($paramname, $delimiter) = @_;

    return join "", map {
        my $s = ucfirst $_;
        $s =~ s/ //g;
        $_ = $s } split $delimiter, $paramname;
}

##
#   generateTypePrefix :
#
#   generate a unique name prefix for deriving
#   types specific to a single endpoint and method
#
#   strip curly braces, parameters, '/' and '-' characters from the endpoint path.
#   uppercase the first character of every string trailing '/' or '-'.
##
sub generateTypePrefix
{
    my ($s) = @_;

    # ensure the string starts with an alpha
    $s =~ s/^[^A-Z]+//i;

    # return the string verbatim if it does not contain delimiter characters
    # it is then the caller's responsibility to ensure lower camel case compliance

    return $s if $s !~ /[-\/]/;

    $s =  lc $s;
    $s =~ s/{([^}]+)}/$1/g;
    $s =~ s/\?.*$//g;

    $s = Parser::formatCamelCase($s, "[^A-Za-z0-9]+");

    return $s;
}

##
#   addCamelSuffix :
#
#   add a camelcase-compliant suffix to typePrefix
##
sub addCamelSuffix
{
    my ($typePrefix, $suffix) = @_;

    $suffix = ucfirst $suffix;
    return $suffix if $typePrefix eq "";

    return Parser::formatCamelCase($typePrefix . $suffix, "[^A-Za-z0-9]+");
}

##
#   addImportedClass
#
#   add a class to a list of imported classes
#   suppressing redundant items
#   (e.g. inbuilt classes, alias references)
##
sub addImportedClass
{
    my ($classimports, $datatype) = @_;

    $classimports = {} if !defined $classimports;

    if (exists $symboltable->{$datatype})
    {
        my $sref = $symboltable->{$datatype};
        return $classimports if exists $sref->{isAlias};
    }

    if (exists $inbuilttypes->{$datatype})
    {
        return $classimports;
    }

    die "INTERNAL: can't import a non-existing symbol" if !exists $symboltable->{$datatype};

    $classimports->{$datatype} = 1;

    return $classimports;
}

##
#   sortResponseCodes :
#
#   numerically sort the response codes
##
sub sortResponseCodes
{
    my ($responses) = @_;

    my @responsekeys        = sort grep !/default/, keys %$responses;
    my @sortedResponseCodes = sort {$a <=> $b} @responsekeys;
    push @sortedResponseCodes, 'default' if exists $responses->{default};
    return @sortedResponseCodes;
}

sub processScalarType
{
    my ($level, $typePrefix, $attribname, $parenttype, $forceEntry) = @_;

    my $filename = 'swagger file';
    $filename = basename($configoptions->{inputfile}) if exists $configoptions->{inputfile};

    die "[$filename] expected 'typePrefix' for attribute \'$attribname\'" if !defined $typePrefix;

    my $result;

    SWITCH:
    {
        ##
        #   process a reference to one of the atomic datatypes
        ##
        (exists $inbuilttypes->{$parenttype->{type}}) && do
        {
            $result = { name => $attribname, definition => $parenttype };
            if (exists $parenttype->{format} && $parenttype->{format} eq 'binary')
            {
                $result->{isBinary} = 'true';
            }
            last SWITCH;
        };
        ##
        #   create a 'string' attribute for an 'additionalProperties' enum declaration
        ##
        (exists $parenttype->{type} && $parenttype->{type} eq 'object' && exists $parenttype->{enum}) && do
        {
            $result = { name => $attribname, definition => { type => 'string' } };
            last SWITCH;
        };
        ##
        #   create a dummy 'object' parameter for an empty 'additionalProperties' declaration
        ##
        (exists $parenttype->{type} && $parenttype->{type} eq 'object' && !exists $parenttype->{additionalProperties}) && do
        {
            $result = { name => $attribname, definition => { type => 'object' }, isDefaulted => 'true' };
            last SWITCH;
        };
        (exists $parenttype->{type} && $parenttype->{type} eq 'null') && do
        {
            $result = { name => $attribname, definition => { type => 'null' } };
            last SWITCH;
        };
        do {
            my $subtype = $parenttype->{type};
            die "[$filename] unexpected scalar subtype $subtype for attribute $attribname";
        };
    }

    return $result;
}

sub processArray
{
    my ($level, $typePrefix, $parenttypename, $parenttype, $forceEntry) = @_;

    my $filename = 'swagger file';
    $filename = basename($configoptions->{inputfile}) if exists $configoptions->{inputfile};

    die "[$filename] \$parenttype->{items} not found for $parenttypename" if !exists $parenttype->{items};

    my $result = processSingleItem($level + 1, $typePrefix, $parenttypename, $parenttype->{items}, 1);

    ##
    #   create a dedicated element type if it has more than one attribute
    ##
    if (exists $result->{attriblist} && scalar @{$result->{attriblist}} > 1)
    {
        my $elementtypename = $result->{name};
        my $arraytypename   = $elementtypename . "ArrayType";

        $result->{name}     = $elementtypename;
        SymbolTable::createSymboltableEntry($result);

        $result             = SymbolTable::createSymboltableReference($elementtypename);
        $result->{name}     = $arraytypename;
    }

    $result->{type} = 'array';

    ##
    #   copy over additional attributes to the array symbol table entry
    ##
    map {$result->{$_} = $parenttype->{$_};} grep !/items|type/, keys %$parenttype;

    return $result;
}

##
#   copyPolymorphProperties
#
#   copy items from a 'properties' list to the attribute list
##
sub copyPolymorphProperties
{
    my ($level, $typePrefix, $parenttypename, $properties, $attriblist, $classimports) = @_;

    for my $propertyname (sort keys %$properties)
    {
        my $property = $properties->{$propertyname};
        my $result   = processSingleItem($level + 1, $typePrefix, $propertyname, $property, 0);

        if (exists $result->{isDefaulted})
        {
            # print "found!\n";
            my $dummy = 1;
        }
        else
        {
            $result->{name} = $propertyname;
            push @$attriblist, $result;
            my $propertyclassname = SymbolTable::resolveDatatypeName($result);
            $classimports = addImportedClass($classimports, $propertyclassname) if defined $propertyclassname;
        }
    }

    return $attriblist;
}

##
#   mergeReferencedClass
#
#   copy items from the attribute list of an existing symbol
#   to the attribute list of the polymorph object.
##
sub mergeReferencedClass
{
    my ($level, $typePrefix, $parenttypename, $reference, $attriblist, $classimports) = @_;

    my $entry = SymbolTable::resolveSymboltableReference($reference);

    SWITCH:
    {
        (exists $entry->{definition}) && do
        {
            push @$attriblist, $entry;
            last SWITCH;
        };
        (exists $entry->{reference}) && do
        {
            push @$attriblist, $entry;
            $classimports = addImportedClass($classimports, $entry->{reference});
            last SWITCH;
        };
        (exists $entry->{attriblist}) && do
        {
            for my $attrib (@{$entry->{attriblist}})
            {
                push @$attriblist, $attrib;
                $classimports = addImportedClass($classimports, $attrib->{reference}) if exists $attrib->{reference};
            }
            last SWITCH;
        };
        do
        {
            die "mergeReferencedClass : unsupported copy operation";
        };
    }

    return $attriblist;
}

sub processPolymorphType
{
    my ($level, $typePrefix, $parenttypename, $parenttype, $forceEntry) = @_;

    my $filename = 'swagger file';
    $filename = basename($configoptions->{inputfile}) if exists $configoptions->{inputfile};

    # if ( $parenttypename eq 'HostConfig' )
    # {
    #     printf "found!\n";
    # }

    my $inheritancetype = extractPolymorphType($parenttype);
    if (scalar @$inheritancetype != 1)
    {
        die "[$filename] number of polymorph subtypes must be exactly 1 for attribute $parenttypename";
    }

    $inheritancetype  = $inheritancetype->[0];

    my $childtype     = $parenttype->{$inheritancetype};

    my $syntheticName = Parser::addCamelSuffix($typePrefix, $parenttypename);
    my $result        = { name => $syntheticName };

    die "[$filename] polymorph childtype does not resolve to an array reference for attribute $parenttypename" if ref $childtype ne "ARRAY";

    my $baseType;
    my $attriblist    = [];
    my $classimports  = {};

    my $doExtendBaseType = MAIN::checkOption($configoptions, 'extendBaseType');

    my $state         = 0;
    for my $mergedAttr (@$childtype)
    {
        if ($state == 0)
        {
            ##
            #   create the basetype of the polymorph object
            ##
            my $basetypename = $parenttypename;
            $basetypename    = Parser::addCamelSuffix($basetypename, 'base-type')                             if $doExtendBaseType;

            $baseType        = processSingleItem($level + 1, $typePrefix, $basetypename, $mergedAttr, 0);

            mergeReferencedClass($level, $typePrefix, $parenttypename, $baseType, $attriblist, $classimports) if !$doExtendBaseType;

            $classimports    = addImportedClass($classimports, $baseType->{name})                             if $doExtendBaseType;
        }
        else
        {
            ##
            #   add attributes to the derived type
            ##
            SWITCH:
            {
                (exists $mergedAttr->{properties}) && do
                {
                    copyPolymorphProperties($level, $typePrefix, $parenttypename, $mergedAttr->{properties}, $attriblist, $classimports);
                    last SWITCH;
                };
                (exists $mergedAttr->{'$ref'}) && do
                {
                    my $subtype = processSingleItem($level + 1, $typePrefix, $parenttypename, $mergedAttr, 0);
                    mergeReferencedClass($level, $typePrefix, $parenttypename, $subtype, $attriblist, $classimports);
                    last SWITCH;
                };
                (exists $mergedAttr->{type} && $mergedAttr->{type} eq 'object') && do
                {
                    warn "[$filename] empty extended attribute '$parenttypename'";
                    last SWITCH;
                };
                do
                {
                    die "processPolymorphType: unsupported extended type in '$parenttypename'";
                };
            }
        }
        $state = 1;
    }

    ##
    #   fall back to regular type if polymorph type lists only the extended class
    ##
    if (scalar @$attriblist > 1 || $doExtendBaseType)
    {
        $result->{composed} = $inheritancetype;
    }

    if ($doExtendBaseType)
    {
        $result->{extendedbasetype} = $baseType;
    }

    $result->{attriblist}   = $attriblist;
    $result->{classimports} = $classimports;

    ##
    #   patch the merged attribute lists into the parent type
    #   if we don't use the 'extends' pattern.
    #   CAVEAT: be careful not to create circular references.
    ##
    if (!$doExtendBaseType)
    {
        $result = SymbolTable::createSymboltableEntry($result);
    }

    $result = SymbolTable::createSymboltableEntry($result);
    $result = SymbolTable::createSymboltableReference($result->{name});

    return $result;
}

sub processSchema
{
    my ($level, $typePrefix, $parenttypename, $parenttype, $forceEntry) = @_;

    my $result = processSingleItem($level + 1, $typePrefix, $parenttypename, $parenttype->{schema}, 0);

    return $result;
}

sub processLinks
{
    my ($level, $typePrefix, $parenttypename, $parenttype, $forceEntry) = @_;

    my $filename = 'swagger file';
    $filename = basename($configoptions->{inputfile}) if exists $configoptions->{inputfile};

    my $links = $parenttype->{links};

    die "[$filename] 'links' must be a hash ref" if ref $links ne 'HASH';

    my $result = {};
    for my $ref (keys %$links)
    {
        $result = processSingleItem($level + 1, $typePrefix, $ref, $links->{$ref}, 0);
    }

    if ((exists $result->{type} && $result->{type} eq 'array') && exists $result->{reference})
    {
        $result->{name} = $parenttypename;
        $result = SymbolTable::createSymboltableEntry($result);
        $result = SymbolTable::createSymboltableReference($parenttypename);
    }

    return $result;
}

sub processContent
{
    my ($level, $typePrefix, $parenttypename, $parenttype, $forceEntry) = @_;

    my $filename = 'swagger file';
    $filename = basename($configoptions->{inputfile}) if exists $configoptions->{inputfile};

    my $content = $parenttype->{content};

    die "[$filename] 'content' must be a hash ref" if ref $content ne 'HASH';

    my $result = {};
    for my $contenttype (keys %$content)
    {
        $result = processSingleItem($level + 1, $typePrefix, $parenttypename, $content->{$contenttype}, 0);
    }

    if (exists $result->{type} && $result->{type} eq 'array' && exists $result->{reference})
    {
        $result->{name} = $parenttypename;
        $result = SymbolTable::createSymboltableEntry($result);
        $result = SymbolTable::createSymboltableReference($parenttypename);
    }

    return $result;
}

sub processProperties
{
    my ($level, $typePrefix, $parenttypename, $parenttype, $forceEntry) = @_;

    # if ($parenttypename eq "IPAM")
    # {
    #     printf "found!\n";
    # }

    my $filename = 'swagger file';
    $filename = basename($configoptions->{inputfile}) if exists $configoptions->{inputfile};

    die "[$filename] expected 'typePrefix' for attribute \'$parenttypename\'" if !defined $typePrefix;
    if (!exists $parenttype->{properties})
    {
        die "[$filename] 'properties' missing in attribute \'$parenttypename\'";
    }

    my $properties        = $parenttype->{properties};

    ##
    #   collect all elements of the composite type
    #   add optional after required attributes
    ##
    my $attribcardinality = {};
    map {
        $attribcardinality->{$_} = 'todo';
    } keys %$properties;

    if (exists $parenttype->{required})
    {
        for my $property (@{$parenttype->{required}})
        {
            die "[$filename] type $parenttypename : required property $property was not declared" if !exists $attribcardinality->{$property};
            $attribcardinality->{$property} = 'mandatory';
        }
    }

    for my $property (keys %$properties)
    {
        $attribcardinality->{$property} = 'optional' if !exists $attribcardinality->{$property} || $attribcardinality->{$property} eq 'todo';
    }

    ##
    #   recursively descend into the child data types
    ##
    my $classimports = {};
    my $attriblist   = [];

    for my $propertyname (sort keys %$properties)
    {
        my $property = $properties->{$propertyname};

        my $tp = $typePrefix;
        $tp = $parenttypename if $tp eq "";
        my $attribref          = processSingleItem($level + 1, $tp, $propertyname, $property, 0);

        ##
        #   ensure the type is tied to the parameter by name
        ##
        $attribref->{name}     = $propertyname;
        $attribref->{name}     = Parser::formatCamelCase($propertyname, "[^A-Za-z0-9]+") if $propertyname =~ /[^A-Za-z0-9]+/;
        $attribref->{required} = $attribcardinality->{$propertyname};

        push @$attriblist, $attribref;

        $property->{classdefinition} = $attribref;

        ##
        #   only add non-alias symboltable entries to the class import list
        ##
        $classimports = addImportedClass($classimports, $attribref->{reference}) if exists $attribref->{reference};

        ##
        #   import class(es) referred to in hash table
        ##
        if (exists $attribref->{isMap} && exists $attribref->{classimports})
        {
            map {
                $classimports = addImportedClass($classimports, $_);
            } keys %{$attribref->{classimports}};
        }
    }

    ##
    #   add a dummy element to an empty property list.
    #   this class will be removed from model class generation and response clauses.
    ##
    my $isDummyClass = 0;
    if (scalar keys %$properties == 0)
    {
        push @$attriblist, { name => 'dummy', definition => { type => 'string' }, isDummy => 'true' };
        $isDummyClass = 1;
    }

    my $syntheticName = $parenttypename;
    $syntheticName    = Parser::addCamelSuffix($typePrefix, $parenttypename);

    my $result        = {};

    $result = { name => $syntheticName, attriblist => $attriblist, classimports => $classimports };
    $result->{isDummyClass} = 'true' if $isDummyClass;

    SymbolTable::createSymboltableEntry($result);

    $result = SymbolTable::createSymboltableReference($syntheticName);

    return $result;
}

sub processAdditionalProperties
{
    my ($level, $typePrefix, $parenttypename, $parenttype, $forceEntry) = @_;

    # if ($parenttypename eq "PortMap")
    # {
    #     printf "found!\n";
    # }

    my $filename = 'swagger file';
    $filename = basename($configoptions->{inputfile}) if exists $configoptions->{inputfile};

    die "[$filename] expected 'typePrefix' for attribute \'$parenttypename\'" if !defined $typePrefix;
    die "[$filename] 'additionalProperties' missing in attribute \'$parenttypename\'" if !exists $parenttype->{additionalProperties};

    my $additionalProperties = $parenttype->{additionalProperties};

    ##
    #   catch broken swagger files
    ##
    if (ref $additionalProperties ne 'HASH')
    {
        warn "[$filename] unsupported type of 'additionalProperties' for attribute $parenttypename";
        return { name => $parenttypename, definition => { type => 'string' }, isDefaulted => 'true' };
    }

    my $temp = processSingleItem($level + 1, $typePrefix, $parenttypename, $additionalProperties, 0);

    my $result = {
        name        => $parenttypename,
        type        => 'object',
        isMap   => 'true',
        hashKeyType => 'string',
        hashValType => $temp
    };

    if (exists $temp->{reference})
    {
        my $classimports = { $temp->{reference} => 1 };
        $result->{classimports} = $classimports;
    }

    # 'additionalProperties' are mapped to Map<String,objecttype>
    # so we don't require a symbol table entry unless it is a 1st level
    # entry from the swagger definitions.
    if ( $forceEntry )
    {
        $result = SymbolTable::createSymboltableEntry($result);
        $result = SymbolTable::createSymboltableReference($parenttypename);
    }

    return $result;
}

sub processParameters
{
    my ($level, $typePrefix, $parenttypename, $parenttype, $forceEntry) = @_;

    my $filename = 'swagger file';
    $filename = basename($configoptions->{inputfile}) if exists $configoptions->{inputfile};

    die "[$filename] expected 'typePrefix' for attribute \'$parenttypename\'" if !defined $typePrefix;
    if (!exists $parenttype->{parameters})
    {
        die "[$filename] 'parameters' missing in attribute \'$parenttypename\'";
    }

    my $parameters     = $parenttype->{parameters};

    ##
    #   collect all elements of the composite type
    #   add optional after required attributes
    ##

    my $classimports   = {};
    my $attriblist     = [];

    for my $parametername (sort keys %$parameters)
    {
        my $parameter = $parameters->{$parametername};
        my $tp        = $typePrefix;
        $tp           = $parenttypename if $tp eq "";
        push @$attriblist, { name => $parametername, definition => { type => 'string' }, description => $parameter };
    }

    my $syntheticName = $parenttypename;
    $syntheticName    = Parser::addCamelSuffix($typePrefix, $parenttypename);

    my $result        = {};

    $result = { name => $syntheticName, attriblist => $attriblist, classimports => $classimports };
    SymbolTable::createSymboltableEntry($result);
    $result = SymbolTable::createSymboltableReference($syntheticName);

    return $result;
}

sub processReference
{
    my ($level, $typePrefix, $parenttypename, $parenttype, $forceEntry) = @_;

    my $childtype = SwaggerUtils::resolveDataTypeReference($parenttype);

    my $name      = $childtype->{name};
    my $result    = $symboltable->{$name};

    return SymbolTable::createSymboltableReference($name) if exists $childtype->{isSymbolDefined};

    $symboltable->{$name}{status} = 'open'; # put a marker into the symbol table that this type is undergoing analysis

    $result       = processSingleItem($level + 1, "", $name, $childtype, 0);

    return $result if exists $result->{reference};

    $result       = SymbolTable::createSymboltableEntry($result);

    ##
    #   return a reference to a symbol table entry to avoid
    #   cluttering it with 'array' tags from anonymous arrays.
    ##
    $result       = SymbolTable::createSymboltableReference($result->{name});

    return $result;
}

##
#   processSingleItem :
#
#   process all flavours of swagger type declaration and
#   map them to a single type of symbol table entry.
##
sub processSingleItem
{
    my ($level, $typePrefix, $itemname, $itemtype, $forceEntry) = @_;

    my $filename = 'swagger file';
    $filename = $configoptions->{inputfile} if exists $configoptions->{inputfile};

    ##
    #   If we have multiple input files :
    #
    #   backup the current type definition before processing the new request.
    #   we verify if the definitions are equivalent at the end of processing
    #   and either abort processing or proceed with the 1st definition.
    ##
    my $clonedCopy = {};

    if ($level > 0 && exists $symboltable->{$itemname})
    {
        my $sref = $symboltable->{$itemname};

        if ($sref->{status} eq 'closed' && $sref->{filename} ne $filename)
        {
            $clonedCopy = { verify => 1, clone => dclone($sref) };
            delete $symboltable->{$itemname};
        }
    }

    $forceEntry = 0 if !defined $forceEntry;

    ##
    #   resolve indirection of 'type' attribute.
    ##
    if (exists $itemtype->{type} && ref $itemtype->{type} eq 'HASH')
    {
        $itemtype = $itemtype->{type};
    }

    if (ref $itemtype ne 'HASH')
    {
        die "$itemname is not a hash reference";
    }

    ##
    #   perform consistency checks on type definition
    ##
    if (exists $itemtype->{type} && ref $itemtype->{type} eq "HASH")
    {
        die "hash table as 'type' attribute of $itemname not supported";
    }

    my $result;

    SWITCH:
    {
        ##
        #   process empty schemes
        ##
        (scalar keys %$itemtype == 0) && do
        {
            $result = { name => $itemname, definition => { type => 'string' }, isDefaulted => 'true' };
            last SWITCH;
        };
        ##
        #   OpenAPI 3.0 has a new way of declaring arrays
        ##
        (exists $itemtype->{type} && ref $itemtype->{type} eq 'ARRAY') && do
        {
            my $temp = $itemtype->{type};
            $temp    = extractNonNullElement($temp);
            if (exists $inbuilttypes->{$temp})
            {
                $temp   = 'string' if $temp eq 'null';
                $result = { name => $itemname, definition => { type => $temp } };
            }
            elsif ($temp eq 'array')
            {
                $result = processArray($level + 1, $typePrefix, $itemname, $itemtype, 0);
            }
            else
            {
                die "unexpected subtype of $itemname";
            }
            last SWITCH;
        };
        (exists $itemtype->{content} && ref $itemtype->{content} eq 'HASH' && !MAIN::checkOption($configoptions, 'enforceLinksResolution')) && do
        {
            $result = processContent($level + 1, $typePrefix, $itemname, $itemtype, 0);
            last SWITCH;
        };
        ##
        #   resolve direct reference to definitions with 'links'
        #   ('links' are available from swagger 2.0 onward)
        ##
        (exists $itemtype->{links} && MAIN::checkOption($configoptions, 'enforceLinksResolution')) && do
        {
            $result = processLinks($level + 1, $typePrefix, $itemname, $itemtype, 0);
            last SWITCH;
        };
        ##
        #   resolve direct reference to definitions with 'schema'
        ##
        (exists $itemtype->{schema}) && do
        {
            $result = processSchema($level + 1, $typePrefix, $itemname, $itemtype, 0);
            last SWITCH;
        };
        ##
        #   resolve direct reference to definitions with '$ref'
        ##
        (exists $itemtype->{'$ref'}) && do
        {
            # for definition table entries we restart at level 0,
            # since this will become a standalone symbol table entry.
            # we reset the typePrefix as well (in sync with the level).
            $result = processReference(0, "", $itemname, $itemtype, 0);
            last SWITCH;
        };
        ##
        #   resolve direct reference to definitions with 'properties'
        ##
        (exists $itemtype->{properties}) && do
        {
            $result = processProperties($level + 1, $typePrefix, $itemname, $itemtype, 0);
            last SWITCH;
        };
        ##
        #   resolve direct reference to definitions with 'properties'
        ##
        (exists $itemtype->{additionalProperties}) && do
        {
            die "$itemname : type='object' not found" if !exists $itemtype->{type} || ref $itemtype->{type} ne '' || $itemtype->{type} ne 'object';
            $result = processAdditionalProperties($level + 1, $typePrefix, $itemname, $itemtype, 0);
            last SWITCH;
        };
        ##
        #   process an array declaration
        ##
        (exists $itemtype->{type} && $itemtype->{type} eq 'array') && do
        {
            $result = processArray($level + 1, $typePrefix, $itemname, $itemtype, 0);
            last SWITCH;
        };
        ##
        #   parse polymorph composite datatype
        ##
        (ref extractPolymorphType($itemtype) eq 'ARRAY') && do
        {
            $result = processPolymorphType($level + 1, $typePrefix, $itemname, $itemtype, 1);
            last SWITCH;
        };
        ##
        #   resolve direct reference to definitions with 'object'.
        #   CAVEAT: this clause must remain behind processPolymorphType()
        ##
        (exists $itemtype->{type} && ref $itemtype->{type} eq '') && do
        {
            $result = processScalarType($level + 1, $typePrefix, $itemname, $itemtype, 0);
            last SWITCH;
        };
        (exists $itemtype->{parameters} && ref $itemtype->{parameters} eq 'HASH') && do
        {
            $result = processParameters($level + 1, $typePrefix, $itemname, $itemtype, 0);
            last SWITCH;
        };
        do {
            die "[$filename] unexpected attribute type for attribute $itemname";
        };
    }

    my $symref             = $result;
    $symref                = SymbolTable::resolveSymboltableReference($result) if exists $result->{reference};

    $symref->{description} = $itemtype->{description} if exists $itemtype->{description};
    $symref->{example}     = $itemtype->{example}     if exists $itemtype->{example};
    $result->{isAlias}     = $symref->{isAlias}       if exists $symref->{isAlias};

    if (exists $symref->{definition} && exists $symref->{definition}{enum})
    {
        $symref->{defaultvalue} = CodeGenerator::extractEnumDefaultValue($symref);
    }

    if (defined $forceEntry && $forceEntry)
    {
        if (!exists $result->{definition} && !exists $result->{reference} && !exists $result->{status} && !exists $symboltable->{$itemname})
        {
            $result = SymbolTable::createSymboltableEntry($result);
        }
    }

    ##
    #   verify if type declarations from different files are equivalent
    ##
    if (exists $clonedCopy->{verify})
    {
        my $mismatch = 0;

        if (exists $symboltable->{$itemname})
        {
            my $sref  = $symboltable->{$itemname};
            $mismatch = findMismatches($itemname, $clonedCopy->{clone}, $sref) > 0;
            die "abort after type mismatch" if $mismatch && !MAIN::checkOption($configoptions, 'ignoreMismatches');
        }

        if (!$mismatch)
        {
            # fall back to the 1st occurence of this type
            delete $symboltable->{$itemname};
            $symboltable->{$itemname} = $clonedCopy->{clone};
        }
    }

    return $result;
}

##
#   processAttribute :
#
#   process a single (primitive or composite) attribute.
##
sub processAttribute
{
    my ($typePrefix, $parenttypename, $parenttype, $exposed) = @_;

    my $result = processSingleItem(0, $typePrefix, $parenttypename, $parenttype, 1);

    ##
    #   tag the symbol table entry as 'exposed'.
    ##
    if (exists $result->{reference} && defined $exposed)
    {
        my $symref         = SymbolTable::resolveSymboltableReference($result);
        $symref->{exposed} = $exposed;

        ##
        #   propagate 'exposed' property to child types, so that
        #   we have class instantiation code available for them.
        #
        #   CAVEAT: this (intentionally) does not recurse over the
        #           dependency graph. if it did, we could just
        #           generate instantiation code for all classes.
        ##
        if (exists $symref->{classimports})
        {
            map {
                $symboltable->{$_}{exposed} = $exposed
            } keys %{$symref->{classimports}};
        }
    }

    return $result;
}

##
#   mergeBodyParameter
#
#   merge the contents of additional in:body parameters to the initial symbol table entry.
##
sub mergeBodyParameter
{
    my ($dst, $classdefinition) = @_;

    SWITCH:
    {
        (exists $classdefinition->{attriblist}) && do
        {
            my $src = $classdefinition->{attriblist};

            map {
                push @{$dst->{attriblist}}, $_;
            } @$src;

            map {
                $dst->{attriblist}{$_} = 1;
            } keys %{$src->{classimports}};

            last SWITCH;
        };
        (exists $classdefinition->{definition}) && do
        {
            push @{$dst->{attriblist}}, $classdefinition;
            last SWITCH;
        };
        do {
            die "[!!!!!!!!] unsupported merge operation";
        }
    }

    return $dst;
}

##
#   processEndPointParameters :
#
#   process endpoint parameters.
#   each parameter is linked to its resolved datatype.
#   this can either be a symbol table entry or a java datatype.
##
sub processEndPointParameters
{
    my ($endpoint, $method, $parameters) = @_;

    if (MAIN::checkOption($configoptions, 'reportonly', 'flags'))
    {
        printf "%s\n", '-' x 132;
        printf "Parameters :\n";

        if (!defined $parameters)
        {
            printf "[%-8s] %-45s[%-9s] : %s\n"
                , " "
                , "no parameters defined"
                , " "
                , " ";

            return;
        }
    }

    my $requestBody;

    for my $param (@$parameters)
    {
        my $classdefinition;
        my $typePrefix = generateTypePrefix("${endpoint}-${method}");

        ##
        #   the only legal way to have '$param->{in}' missing is
        #   having a parameter resolve to an external reference.
        ##
        if (!exists $param->{in})
        {
            die "reference expected while missing {in} : ${method}::${endpoint}" if !exists $param->{'$ref'};
            $param = SwaggerUtils::resolveDataTypeReference($param);
            $param = $param->{type};
        }

        SWITCH:
        {
            ($param->{in} =~ m/body|default/) && do {

                my $paramname    = " ";
                $paramname       = $param->{name} if exists $param->{name};
                $classdefinition = processAttribute($typePrefix, $paramname, $param, 'inbound');

                my $paramtype    = SymbolTable::resolveSymbolAttribTypeName($classdefinition);

                if (!defined $requestBody)
                {
                    $requestBody              = $classdefinition;
                    $param->{classdefinition} = $classdefinition;
                }
                else
                {
                    ##
                    #   merge multiple in:body parameters into a single type.
                    #   as of now, the first parameter must have been resolved into a symboltable entry.
                    ##
                    die "1st request body must resolve into a composite type"     if !exists $requestBody->{name};
                    die "1st request body must resolve into a symbol table entry" if !exists $requestBody->{reference};
                    # printf "[!!!!!!!!] merge multiple inbody parts in %s\n", exists $configoptions->{inputfile} ? basename($configoptions->{inputfile}) : "swagger file";

                    mergeBodyParameter($symboltable->{$requestBody->{reference}}, $classdefinition);

                    ##
                    # earmark the parameter so that the generation functions will skip it.
                    ##
                    $param->{isMerged} = 'true';
                }

                if (MAIN::checkOption($configoptions, 'reportonly', 'flags'))
                {
                    if (ref $paramtype ne "")
                    {
                        my $temp   = "<undefined>";
                        $temp      = sprintf("polymorph : %s", $paramtype->{composed}) if exists $paramtype->{derived};
                        $paramtype = $temp;
                    }

                    printf "[%-8s] %-45s[%-9s] : %s\n"
                        , $param->{in}
                        , $paramtype
                        , exists $param->{required} ? "mandatory" : "optional"
                        , $paramname;
                }

                last SWITCH;
            };
            ($param->{in} =~ m/header|path|query|formData/) && do {

                $classdefinition          = processAttribute($typePrefix, "", $param, 'inbound');
                $param->{classdefinition} = $classdefinition;

                if (MAIN::checkOption($configoptions, 'reportonly', 'flags'))
                {
                    my $paramname = " ";
                    $paramname    = $param->{name} if exists $param->{name};

                    my $paramtype = SymbolTable::resolveSymbolAttribTypeName($classdefinition);

                    printf "[%-8s] %-45s[%-9s] : %s\n"
                        , $param->{in}
                        , $paramtype
                        , exists $param->{required} ? "mandatory" : "optional"
                        , $paramname;
                }
                last SWITCH;
            };
            do {
                die "$param->{name} : unsupported parameter type $param->{in}";
            };
        }
    }
}

##
#   processEndPointResponses :
#
#   process endpoint responses.
#   each response is linked to its resolved datatype.
#   this can either be a symbol table entry or a java datatype.
##
sub processEndPointResponses
{
    my ($endpoint, $method, $responses) = @_;

    if (MAIN::checkOption($configoptions, 'reportonly', 'flags'))
    {
        printf "%s\n", '-' x 72;
        printf "Responses  :\n";
    }

    # if ($endpoint eq "/suppression/spam_reports")
    # {
    #     printf "found!\n";
    # }

    my @sortedResponseCodes = sortResponseCodes($responses);

    for my $key (@sortedResponseCodes)
    {
        my $response    = $responses->{$key};
        my $classdefinition;

        my $typePrefix  = "";
        my $typeName    = "";
        if (MAIN::checkOption($configoptions, 'useErrorResponse') && $key !~ /^[12]/)
        {
            $typePrefix = 'ErrorResponse';
            $typeName   = "";
        }
        else
        {
            $typePrefix = generateTypePrefix("${endpoint}-${method}");
            $typeName   = "Rc${key}";

            #$typeName = Parser::addCamelSuffix( $typePrefix, $typeName );
            # if (exists $response->{schema}{title})
            # {
            #     $typeName = $response->{schema}{title};
            # }
        }

        SWITCH:
        {
            (exists $response->{schema}) && do
            {
                my $typeref      = $response->{schema};
                $classdefinition = processAttribute(ucfirst $typePrefix, $typeName, $typeref, 'outbound');
                last SWITCH;
            };
            (exists $response->{content} && !MAIN::checkOption($configoptions, 'enforceLinksResolution')) && do
            {
                my $typeref      = $response;
                $classdefinition = processAttribute(ucfirst $typePrefix, $typeName, $typeref, 'outbound');
                last SWITCH;
            };
            (exists $response->{links} && MAIN::checkOption($configoptions, 'enforceLinksResolution')) && do
            {
                my $typeref      = $response;
                $classdefinition = processAttribute(ucfirst $typePrefix, $typeName, $typeref, 'outbound');
                last SWITCH;
            };
            do
            {
                if (MAIN::checkOption($configoptions, 'reportonly', 'flags'))
                {
                    printf "[%-8s] %-45s %-9s \n", $key, "schema missing (default: void)", " ";
                }
                $classdefinition = { definition => { type => 'void' } };
                last SWITCH;
            };
        }

        $response->{classdefinition} = $classdefinition;

        if (MAIN::checkOption($configoptions, 'reportonly', 'flags'))
        {
            my $responsetype = SymbolTable::resolveSymbolAttribTypeName($classdefinition);
            my $description  = CodeGenerator::formatDescriptionAsOneLine($response->{description}, 72, { dontescapeapostrophes => 'true' }) if exists $response->{description};
            $description     = "" if !defined $description;
            printf "[%-8s] %-45s %-9s  : %s\n", $key, $responsetype, " ", $description;
        }
    }
}

##
#   processEndPointOperation :
#
#   process a single operation for an endpoint.
##
sub processEndPointOperation
{
    my ($endpoint, $method) = @_;

    my $paths = $inputhash->{paths};
    my $pi    = $paths->{$endpoint};
    my $mi    = $pi->{$method};

    if (MAIN::checkOption($configoptions, 'reportonly', 'flags'))
    {
        printf "%s\n", '=' x 132;
        printf "[%-7s] %-25s\n", $method, $endpoint;
        if (exists $mi->{consumes})
        {
            my $consumes = $mi->{consumes};
            printf("consumes : %s\n", array2ConcatenatedStrings($consumes));
        }
        if (exists $mi->{produces})
        {
            my $produces = $mi->{produces};
            printf("produces : %s\n", array2ConcatenatedStrings($produces));
        }
    }

    ##
    #   collect all endpoints in a hash table keyed by the first element of the 'tags' array.
    ##
    if (MAIN::checkOption($configoptions, 'clusterByTag'))
    {
        my $filename = 'swagger file';
        $filename = basename($configoptions->{inputfile}) if exists $configoptions->{inputfile};

        my $tags  = $mi->{tags} if exists $mi->{tags};

        my $tag;
        if (!defined $tags || scalar @$tags < 1)
        {
            $tag = 'Default';
            warn "[$filename] no 'tags' label found for ${method}::${endpoint}. using '$tag'";
        }
        else
        {
            $tag = $tags->[0];
            warn "[$filename] more than 1 'tags' labels found for ${method}::${endpoint}, using '$tag'" if scalar @$tags > 1;
        }

        $taggedEndpoints->{$tag}{$endpoint}{$method} = 1;
    }

    ##
    #   process input parameters types
    ##
    my $parameters = [];
    map {push @$parameters, $_} @{$pi->{parameters}} if exists $pi->{parameters};
    map {push @$parameters, $_} @{$mi->{parameters}} if exists $mi->{parameters};

    processEndPointParameters($endpoint, $method, $parameters);

    ##
    #   process output response types
    ##
    if (exists $mi->{responses})
    {
        processEndPointResponses($endpoint, $method, $mi->{responses});
    }
}

##
#   processEndPoint :
#
#   process either a single endpoint or all endpoints from the input file.
##
sub processEndPoint
{
    my ($endpoint) = @_;

    if ($endpoint =~ m/:/)
    {
        my @s       = split ':', $endpoint;
        $endpoint   = $s[0];
        my $method  = $s[1];
        $collectedEndpoints->{$endpoint}{$method} = 1;
        processEndPointOperation($endpoint, $method);
    }
    else
    {
        map {
            my $method = $_;
            ##
            #  CAVEAT : the same endpoint can be hit with multiple methods,
            #           so our accounting in '$collectedEndpoints' has to
            #           cater for that by using a composite hashkey.
            ##
            $collectedEndpoints->{$endpoint}{$method} = 1;
            processEndPointOperation($endpoint, $method);
        } sort grep !/parameters/, keys %{$inputhash->{paths}{$endpoint}};
    }
}

########################################################################
##
#       Generate Java Classes from Symboltable Entries
##
########################################################################

package CodeGenerator;

use File::Basename;

sub sanitizeDescription
{
    my ($description, $options) = @_;

    my $default = "";
    return $default if !defined $description || $description eq "";

    # replace line delimiters by spaces
    $description =~ s/[\r]/\n/g;
    $description =~ s/\\n/\n/g;
    $description =~ s/[\n]+/ /g;

    # trim leading and trailing whitespace
    $description =~ s/^\s+|\s+$//g;
    # trim leading and trailing apostrophes
    $description =~ s/^["']+|["']+$//g;

    $description =~ s/\.+/./g;
    $description =~ s/[\t]+/ /g;
    $description =~ s/[`´]/'/g;

    $description =~ s/["]/\\"/g if !defined $options || !exists $options->{dontescapeapostrophes};

    return $description;
}

sub formatDescription
{
    my ($indent, $description, $closecomment) = @_;

    return "" if !defined $description || $description eq "";

    $description =~ s/[\r]/\n/g;
    $description =~ s/\\n/\n/g;
    $description =~ s/[\n]+/ ^/g;
    $description =~ s/\.+/./g;
    $description =~ s/[\t]+/ /g;
    $description =~ s/(?![ ]+$)\*/ */g;

    my @words = split /[ ]/, $description;

    my $lines = "";
    my $line  = "${indent}/**";
    my $bol   = 1;

    map {
        my $word = $_;

        SWITCH:
        {
            ($word =~ m/^[\*]+/) && do {
                if (!$bol)
                {
                    $lines .= "$line\n";
                    $line = "${indent} *  $word";
                }
                else
                {
                    $line .= " $word" if length($word) > 0;
                }
                $bol = 0;
                last SWITCH;
            };
            ($word =~ m/^[\^]+/) && do {
                $word =~ s/^[\^]+//;
                if (!$bol)
                {
                    $lines .= "$line\n";
                    $line   = "${indent} * ";
                    $bol    = 1;
                    if (length($word) > 0)
                    {
                        $line .= " $word";
                        $bol   = 0;
                    }
                }
                elsif ($word !~ / +/ && length($word) > 0)
                {
                    $line .= " $word";
                    $bol   = 0;
                }
                last SWITCH;
            };
            (length($line) + length($word) < 80) && do {
                if (length($word) > 0)
                {
                    $line .= " $word";
                    $bol   = 0;
                }
                last SWITCH;
            };
            do {
                $lines .= "$line\n";
                $line   = "${indent} * ";
                $line  .= " $word" if length($word) > 0;
                $bol    = !length($word);
            };
        };
    } @words;

    $closecomment = $closecomment ? ' */' : ' *';
    $lines .= "$line\n" if $line !~ /\/\*\*$/ && $line !~ /\*\ \ $/ && !$bol;
    $lines .= "${indent}${closecomment}\n";

    return $lines;
}

sub formatDescriptionAsOneLine
{
    my ($description, $maxlength, $options) = @_;

    $description = sanitizeDescription($description, $options);
    $description = substr($description, 0, $maxlength - 3) . "..." if defined $description && length($description) > $maxlength - 3;

    return $description
}

sub formatConstraints
{
    my ($typename, $datatype) = @_;

    my $attribconstraints = "";

    return $attribconstraints if !defined $typename;

    if (exists $datatype->{type} && $datatype->{type} eq 'array' && !exists $datatype->{isMap})
    {
        if (!exists $datatype->{definition} && !exists $datatype->{reference} && !exists $datatype->{attriblist})
        {
            die "elementtype missing for array type $typename";
        }
        $attribconstraints .= sprintf " [min: %d]", $datatype->{minItems} if exists $datatype->{minItems};
        $attribconstraints .= sprintf " [max: %d]", $datatype->{maxItems} if exists $datatype->{maxItems};
    }
    elsif (exists $inbuilttypes->{$typename})
    {
        map {
            my $key = $_;
            my $val = $datatype->{definition}{$_};
            $val    = join ', ', @$val if $key eq 'enum';
            $attribconstraints .= sprintf " [%s: %s]", $key, $val;
        } sort grep !/classdefinition|description|type/i, keys %{$datatype->{definition}};
    }

    return $attribconstraints;
}

sub concatAttrNames
{
    my ($attrlist) = @_;

    my $s = '';
    my $delim = '';
    map {
        die "concatAttrNames : attribute is either not a hash or is missing 'name'" if ref($_) ne 'HASH' || !exists $_->{name};
        $s .= ${delim} . $_->{name};
        $delim = ' :: ';
    } @$attrlist;

    return $s;
}

##
#   generateParameterName :
#
#   generate a java-compliant parameter name from a swagger parameter
##
sub generateParameterName
{
    my ($s) = @_;

    # return the string verbatim if it does not contain delimiter characters
    # it is then the caller's responsibility to ensure lower camel case compliance
    return lcfirst Parser::formatCamelCase($s, "[^A-Za-z0-9]+") if $s !~ /[-\/]/;

    $s = lc $s;
    $s = Parser::formatCamelCase($s, "[^A-Za-z0-9]+");

    return lcfirst $s;
}

sub extractJsonCardinality
{
    my ($attr, $values) = @_;

    die "\$attr is not a hash"     if ref $attr ne 'HASH';
    die "\$values is not an array" if ref $values ne 'ARRAY';

    return $values->[1] if !exists $attr->{required};
    return $attr->{required} eq 'mandatory' ? $values->[0] : $values->[1];
}

sub extractParamCardinality
{
    my ($attr, $values) = @_;

    die "\$attr is not a hash"     if ref $attr ne 'HASH';
    die "\$values is not an array" if ref $values ne 'ARRAY';

    return $values->[1] if !exists $attr->{required};
    return exists $attr->{required} && $attr->{required} ? $values->[0] : $values->[1];
}

sub isClassImportSuppressed
{
    my ($classname) = @_;

    $classname = uc $classname;

    return 1 if $classname =~ /^java/i;

    return exists $classesSuppressedFromImport->{$classname};
}

##
#   isEnumType :
#
#   test if a type is a valid enum declaration
##
sub isEnumType
{
    my ($type, $classname) = @_;

    return undef if !exists $type->{definition};

    my $definition = $type->{definition};

    return undef if (exists $definition->{type} && $definition->{type} ne 'string') || !exists $definition->{enum} || ref $definition->{enum} ne 'ARRAY';

    my $enum       = $definition->{enum};

    my $enumValueIsNotScalar = 0;
    map {
        $enumValueIsNotScalar = 1 if ref $_ ne "";
    } @$enum;

    if ($enumValueIsNotScalar)
    {
        my $filename = 'swagger file';
        $filename = basename($configoptions->{inputfile}) if exists $configoptions->{inputfile};

        my $typename;
        $typename = $classname if defined $classname;
        $typename = $type->{name} if exists $type->{name} && !defined $typename;
        $typename = "<unknown>" if !defined $typename;

        warn "[$filename] enum declaration $typename has at least one non-scalar value";

        return undef;
    }

    return $enum;
}

##
#   extractEnumDefaultValue :
#
#   pick a default value for model class instantiation
##
sub extractEnumDefaultValue
{
    my ($type) = @_;

    my $enum = isEnumType($type);

    return undef if !defined $enum;

    my $defaultvalue  = undef;

    for my $val (@$enum)
    {
        $val          =~ s/[^A-Z0-9_]//ig;
        next if $val eq "";
        $defaultvalue = $val if !defined $defaultvalue;
        last if defined $defaultvalue;
    }

    return $defaultvalue;
}

##
#   collectClassAttributeList
#
#   collect the attributes of extended class(es) and the base class
##
sub collectClassAttributeList
{
    my ($classname) = @_;

    my $classentry                    = $symboltable->{$classname};

    my $classattributescomplete       = [];
    my $classattributesbaseclass      = [];

    my $result = {};

    ##
    #   CAVEAT: the super type attributes must precede the attributes of the
    #           derived class in the parameter list. if they don't, the constructor
    #           will cause a compile error.
    #
    #   calling ourselves recursively ensures that daisy-chained 'extends'
    #   relationships will be properly resolved.
    #   CAVEAT: the compounded superclass attributes come from 'classattributescomplete' !
    ##
    if (exists $classentry->{extendedbasetype})
    {
        my $extendedbasetype          = $classentry->{extendedbasetype};
        my $extendedbaseclassname     = $extendedbasetype->{name};
        my $temp                      = collectClassAttributeList($extendedbaseclassname);

        my $classattributessuperclass = [];

        map {
            push @$classattributescomplete, $_;
            push @$classattributessuperclass, $_;
        } @{$temp->{classattributescomplete}};

        $result->{classattributessuperclass} = $classattributessuperclass;
    }

    my $temp = [];

    SWITCH:
    {
        (exists $classentry->{attriblist}) && do
        {
            $temp = $classentry->{attriblist};
            last SWITCH;
        };
        (exists $classentry->{definition}) && do
        {
            push @$temp, $classentry;
            last SWITCH;
        };
        (exists $classentry->{reference}) && do
        {
            push @$temp, $classentry;
            last SWITCH;
        };
        (exists $classentry->{isMap}) && do
        {
            push @$temp, $classentry;
            last SWITCH;
        };
        do {
            die "can't derive attributes for class $classname";
        }
    }

    map {
        push @$classattributescomplete, $_;
        push @$classattributesbaseclass, $_;
    } @$temp;

    $result->{classattributesbaseclass} = $classattributesbaseclass;
    $result->{classattributescomplete}  = $classattributescomplete;

    return $result;
}

########################################################################
#       Generate Model classes
########################################################################

##
#   generateModelInbuiltInstance
#
#   intercept inbuilt datatypes
#   map string attribute to its name, the other inbuilt types to default values
##
sub generateModelInbuiltInstance
{
    my ($level, $attribdelim, $attribname, $classname, $cardinality) = @_;

    my $defaultvalue = undef;
    $defaultvalue = "\"${attribname}\""     if $classname =~ /string/i;
    $defaultvalue = "new BigInteger(\"0\")" if $classname =~ /integer/i;
    $defaultvalue = "new BigDecimal(0.0)"   if $classname =~ /number/i;
    $defaultvalue = "true"                  if $classname =~ /boolean/i;
    ##
    #   use a string instead of a null object to prevent serialization errors
    #   in HashMap<String,null>
    ##
    $defaultvalue = "\"null object\""       if $classname =~ /object/i;
    $defaultvalue = "null"                  if $classname =~ /void/i;
    $defaultvalue = "null"                  if $classname =~ /null/i;

    if (!defined $defaultvalue)
    {
        die "inbuilt default value not derived for attribute $attribname";
    }

    my $dtotypedattribute = sprintf "%-7s %s", $classname, $attribname if defined $cardinality;
    my $is      = ' ' x (4 * $level);
    my $temp    = "${is}${attribdelim}${defaultvalue}";
    my $padlen  = 88 - length($temp);
    $padlen     = 1 if $padlen < 0;
    my $padding = ' ' x $padlen;

    my $output  = $temp;
    if (defined $cardinality)
    {
        $output = <<"EOT";
${temp}${padding}// [$cardinality] ${dtotypedattribute}
EOT
    }

    return $output;
}

sub generateModelEnumInstance
{
    my ($level, $attribdelim, $attribname, $attr, $enumclassname, $classimports, $cardinality) = @_;

    my $enum         = isEnumType($attr);

    die "$attribname is not an enum declaration " if !defined $enum;

    my $defaultvalue = extractEnumDefaultValue($attr);
    my $is           = ' ' x (4 * $level);
    my $temp         = "${is}${attribdelim}${enumclassname}._${defaultvalue}";
    my $padding      = "";
    $padding         = ' ' x (88 - length($temp)) if length($temp) < 88;

    my $output = <<"EOT";
${temp}${padding}// [$cardinality] $attribname
EOT

    return $output;
}

##
#   generateModelMapInstance
#
#   generate an instantiation statement for 2 different flavours of HashMap<K,V> declaration
##
sub generateModelMapInstance
{
    my ($level, $attribdelim, $attribname, $modelpackage, $attr, $classimports, $cardinality) = @_;

    my $is              = ' ' x (4 * $level);

    my $hashKeyType     = $attr->{hashKeyType};
    die "$hashKeyType key type of attribute $attribname is not an inbuilt type" if !exists $inbuilttypes->{$hashKeyType};
    $hashKeyType        = $inbuilttypes->{$hashKeyType};

    my $hashValType     = $attr->{hashValType};
    my $isMapOfList     = exists $hashValType->{type} && $hashValType->{type} eq 'array';

    my $valclassname    = SymbolTable::resolveDatatypeName( $hashValType );

    my $temp            = "${is}${attribdelim}Maps.newHashMap( ImmutableMap.of(";

    my $padding         = "";
    $padding            = ' ' x (88 - length($temp)) if length($temp) < 88;

    my $valInstance;
    my $keyindent;

    ##
    #   process HashMap<String, ArrayList<T>> declarations
    ##
    if ($isMapOfList)
    {
        $valInstance    = generateModelClassInstance($level+2, ' ', "${attribname}.value", $modelpackage, $valclassname, $classimports, 'optional');
        $valclassname   = $inbuilttypes->{$valclassname} if exists $inbuilttypes->{$valclassname};
        $keyindent      = "${is}    ";

        my $arraytype   = "ArrayList<$valclassname>";
        $valInstance    = <<"EOT";
${is}    ,new ${arraytype}(
${is}         Arrays.asList(
${valInstance}${is}     ))
EOT
    }
    else
    {
        $valInstance    = generateModelClassInstance($level+1, ',', "${attribname}.value", $modelpackage, $valclassname, $classimports, 'optional');
        $valInstance    =~ m/^( +)[^ ]/;
        $keyindent      = $1;
    }

    my $output = <<"EOT";
${temp}${padding}// [$cardinality] $attribname
${keyindent} "${attribname}.key"
${valInstance}${is} ))
EOT

    return $output;
}

##
#   generateModelClassInstance :
#
#   recursively step through the nested class definitions to
#   build a constructor for a top-level request datatype.
##
sub generateModelClassInstance
{
    my ($level, $attribdelim, $attribname, $modelpackage, $classname, $classimports, $cardinality) = @_;

    # if ($classname eq "IPAM")
    # {
    #     printf "found!\n";
    # }

    if (exists $inbuilttypes->{$classname})
    {
        return generateModelInbuiltInstance($level, $attribdelim, $attribname, $classname, $cardinality);
    }

    if (!exists $symboltable->{$classname})
    {
        die "$classname is not a defined symbol";
    }

    my $classentry              = $symboltable->{$classname};

    die "can't instantiate standalone array $classname/$attribname" if exists $classentry->{type} && $classentry->{type} eq 'array';

    my $dtoclass                = $classname;
    $dtoclass                   = $inbuilttypes->{$classname}       if exists $inbuilttypes->{$classname};
    my $indent                  = 4 * $level;
    my $is                      = ' ' x $indent;

    $classimports               = {} if !defined $classimports;
    $classimports               = Parser::addImportedClass($classimports, $classname);

    my $output                  = "";
    $output                     = "${dtoclass} request ="           if !$level;
    $output                    .= "${is}${attribdelim}new ${dtoclass}";

    my $attribpadding           = 88 - length($output);
    $attribpadding              = 1 if $attribpadding <= 0;
    $attribpadding              = ' ' x $attribpadding;

    $cardinality                = "" if !defined $cardinality;
    $cardinality                = "[$cardinality] "                 if defined $cardinality;

    $output                    .= "${attribpadding}// ${cardinality}${attribname}";
    $is                        .= ' ' if $level;

    $output                    .= "\n${is}(\n";

    my $delim                   = ' ';
    $is                         = ' ' x ($indent + 4);

    my $classattributes         = collectClassAttributeList($classname);
    my $classattributescomplete = $classattributes->{classattributescomplete};

    ##
    #   CAVEAT : instantiate base and extended class attributes
    ##

    # printf "[%2d] %-45s. List      : %-s\n", $level, $classname, concatAttrNames($classattributescomplete);

    for my $attribref (@$classattributescomplete)
    {
        my $dtoattributename      = $attribref->{name};

        my $dtoattribcardinality  = extractJsonCardinality($attribref, [ "mandatory", "optional " ]);
        my $dtoattributetype      = SymbolTable::resolveDatatypeName($attribref);

        ##
        #   CAVEAT:     reassigning '$attribref' will modify the symbol table entry !
        #   SOLUTION :  insert a temporary variable
        ##
        my $attr                  = $attribref;

        my $isArray               = 0;
        $isArray                  = 1 if exists $attr->{type} && $attr->{type} eq 'array';

        my $dereferencedAttribute = SymbolTable::resolveTypeAlias($attr);
        $attr                     = $dereferencedAttribute->{attribute} if exists $dereferencedAttribute->{attribute};
        $isArray                  = $dereferencedAttribute->{isArray}   if exists $dereferencedAttribute->{isArray};

        # printf "[%2d] %-45s. Attribute : %-40s %s\n", $level, $classname, $dtoattributename, $attr->{name};

        SWITCH:
        {
            ##
            #   CAVEAT: intercept Maps before any other type, since the name does not refer to a symbol table entry
            ##
            (exists $attr->{isMap}) && do
            {
                my $temp = generateModelMapInstance($isArray ? $level+3 : $level+1, $isArray ? ' ' : $delim, $dtoattributename, $modelpackage, $attr, $classimports, $dtoattribcardinality);
                ##
                #   process ArrayList<HashMap<String,T>> declaration
                ##
                if ( $isArray )
                {
                    my $elementtypename = SymbolTable::deriveMapTypeDeclaration($attr, {}, {'usejava' => 'true'});
                    my $arraytype = "ArrayList<$elementtypename>";
                    $temp = <<"EOT";
${is}${delim}new $arraytype(
${is}     Arrays.asList(
${temp}${is} ))
EOT
                }
                $output .= $temp;
                last SWITCH;
            };
            ($isArray) && do
            {
                my $elementtypename    = SymbolTable::resolveDatatypeName($attr);
                $dtoattributetype      = $elementtypename;
                $elementtypename       = $inbuilttypes->{$elementtypename}        if exists $inbuilttypes->{$elementtypename};

                ##
                #   generate a constructor for an ArrayList()
                ##
                my $arraytype = "ArrayList<$elementtypename>";
                my $temp = <<"EOT";
${is}${delim}new $arraytype(
${is}     Arrays.asList(
EOT
                $output .= $temp;

                # relative indentation for ArrayList elements is twice that of 'new'
                $temp = generateModelClassInstance($level + 3, ' ', $dtoattributename, $modelpackage, $dtoattributetype, $classimports, $dtoattribcardinality);
                $output .= $temp;
                $output .= "${is} ))\n";
                last SWITCH;
            };
            (isEnumType($attr)) && do
            {
                my $enumtypename       = $classname;
                my $ucDtoattributename = ucfirst $dtoattributename;
                $enumtypename          = "${classname}.${ucDtoattributename}Enum" if !exists $dereferencedAttribute->{isDereferenced};
                $enumtypename          = $dtoattributetype                        if exists $dereferencedAttribute->{isDereferenced};
                my $temp = generateModelEnumInstance($level + 1, $delim, $dtoattributename, $attr, $enumtypename, $classimports, $dtoattribcardinality);
                $output .= $temp;
                last SWITCH;
            };
            (exists $inbuilttypes->{$dtoattributetype}) && do
            {
                my $temp = generateModelInbuiltInstance($level + 1, $delim, $dtoattributename, $dtoattributetype, $dtoattribcardinality);
                $output .= $temp;
                last SWITCH;
            };
            do
            {
                die "could not derive attribute type for $dtoattributename" if !defined $dtoattributetype;

                ##
                #   propagate the child imports to the parent
                ##
                if (exists $symboltable->{$dtoattributetype}{classimports})
                {
                    map {
                        $classimports = Parser::addImportedClass($classimports, $_);
                    } keys %{$symboltable->{$dtoattributetype}{classimports}};
                }

                ##
                #   generate a constructor for a regular composite datatype
                ##
                my $temp = generateModelClassInstance($level + 1, $delim, $dtoattributename, $modelpackage, $dtoattributetype, $classimports, $dtoattribcardinality);
                $output .= $temp;
            };
        }

        $delim = ',';
    }

    $is      = ' ' x $indent;
    $is     .= ' ' if $level;
    $output .= $is;
    $output .= $level ? ")\n" : ");\n";

    ##
    #   prefix import statements to the generated constructor
    #   if we are at the top-level of the nested types.
    ##
    if (!$level)
    {
        my $dtoclassimports = "";
        my $temp;
        for my $importedclass (sort keys %$classimports)
        {
            next if isClassImportSuppressed($importedclass);
            $importedclass = "${modelpackage}.${importedclass}";
            $temp = <<"EOT";
import ${importedclass};
EOT
            $dtoclassimports .= $temp;
        }

        $output = "$dtoclassimports\n$output";

        ##
        #   wrap the entire code fragment into a java multiline comment.
        ##
        $output = "/*\n${output}*/\n\n";
    }

    return $output;
}

##
#   generateEnumClassDefinition :
#
#   generate 2 flavours of enum class definition :
#   - standalone
#   - embedded (inner class)
##
sub generateEnumClassDefinition
{
    my ($classname, $symref, $ucdtoattributename, $modelpackage, $options) = @_;

    return undef if !defined $classname;

    my $enum            = isEnumType($symref, $classname);

    return undef if !defined $enum;

    $classname          = "${ucdtoattributename}";
    $classname         .= "Enum" if exists $options->{embedded};
    $ucdtoattributename = $classname;

    my $enumvalueclause = "";
    my $delim           = " ";
    my $is              = ' ' x 4;

    for my $val (@$enum)
    {
        $val =~ s/[^A-Z0-9_]//ig;
        next if $val eq "";
        my $temp = <<"EOT";
${is}${delim}_${val}("${val}")
EOT
        $enumvalueclause .= $temp;
        $delim = ",";
    }

    $enumvalueclause .= $is . ";";

    my $enumclassimportclause = "";
    my $xmlconfiguration      = "";

    ##
    #   prefix a standalone class definition
    ##
    if (!exists $options->{embedded})
    {
        my $xmlimportclasses      = "";
        if ( MAIN::checkOption($configoptions, 'xmlSerialization') )
        {
            $xmlconfiguration = "\n\@JacksonXmlRootElement\n";
            $xmlimportclasses = << "EOT";

import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;
EOT
        }

        $enumclassimportclause = <<"EOT";
package ${modelpackage};

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonCreator;${xmlimportclasses}

EOT
    }

    my $enumclassdeclaration = <<"EOT";
${enumclassimportclause}${xmlconfiguration}public enum ${classname}
{
${enumvalueclause}

    private String value;

    ${classname}(String value) { this.value = value; }

    \@Override
    \@JsonValue
    public String toString()
    {
        return String.valueOf(value);
    }

    \@JsonCreator
    public static ${classname} fromValue(String text)
    {
        for (${classname} b : ${classname}.values())
        {
            if (String.valueOf(b.value).equals(text))
            {
                return b;
            }
        }
        return null;
    }
}

EOT

    if (exists $options->{writetofile})
    {
        FileInterface::writeModelClass($configoptions->{model}{path}, $classname, $enumclassdeclaration);
    }

    return { classname => $classname, classdefinition => $enumclassdeclaration };
}

##
# generateClassAttributeClauses
#
# derive bits and pieces for model class attributes and methods from the symbol attrib list
#
# dtoclassajsonattributelist    json-annotated instance variables
# dtoclassinitialisationlist    instance variables initialize to go into the constructor
# gettersettermethods           get and set methods
# dtoclassattributestostring    body of toString() method
#
# dtoclasstypedattributelist    for base type
# dtoclassparmattributelist
#
# xdtoclasstypedattributelist   for super type
# xdtoclassparmattributelist
##
sub generateClassAttributeClauses
{
    my ($classname) = @_;

    my $modelpackage               = $configoptions->{model}{package};

    die "collectClassAttributeClauses : $classname not found in symboltable" if !exists $symboltable->{$classname};

    my $classentry                 = $symboltable->{$classname};

    my $dtoclassajsonattributelist = "";
    my $dtoclasstypedattributelist = "";
    my $dtoclassparmattributelist  = "";
    my $dtoclassinitialisationlist = "";
    my $gettersettermethods        = "";
    my $dtoclassattributestostring = "  ";

    my $ctadelim                   = "";
    my $tosconcat                  = "  ";
    my $tosdelim                   = "";

    my $collectedattributes        = collectClassAttributeList($classname);
    my $classattributesbaseclass   = $collectedattributes->{classattributesbaseclass};

    if ( scalar @$classattributesbaseclass == 0 )
    {
        my $filename = 'swagger file';
        $filename = basename($configoptions->{inputfile}) if exists $configoptions->{inputfile};

        if (exists $classentry->{extendedbasetype})
        {
            my $extendedbasetype      = $classentry->{extendedbasetype};
            my $extendedbaseclassname = $extendedbasetype->{name};

            warn "[$filename] class $classname extends $extendedbaseclassname but has no attributes of its own.";
        }
        else
        {
            die "[$filename] INTERNAL : class $classname has no attributes.";
        }
    }

    ##
    #   CAVEAT : only process base class attributes
    ##
    for my $attribref (@$classattributesbaseclass)
    {
        my $dtoattributename      = $attribref->{name};

        ##
        #   inject a temporary variable to avoid trashing the symbol table
        #   when reassigning $attribref.
        ##
        my $attr                  = $attribref;
        my $isArray               = exists $attr->{type} && $attr->{type} eq 'array';
        my $dereferencedAttribute = SymbolTable::resolveTypeAlias($attr);

        $attr                     = $dereferencedAttribute->{attribute} if exists $dereferencedAttribute->{attribute};
        $isArray                  = $dereferencedAttribute->{isArray}   if exists $dereferencedAttribute->{isArray};

        my $dtoattributetype      = SymbolTable::resolveDatatypeName($attr);

        my $lcdtoattributename    = lcfirst generateParameterName($dtoattributename);
        $lcdtoattributename       = "_$lcdtoattributename";
        my $ucdtoattributename    = ucfirst generateParameterName($dtoattributename);

        my $enumclassdeclaration  = "";
        my $enumclassinfo         = generateEnumClassDefinition($dtoattributetype, $attr, $ucdtoattributename, $modelpackage, { embedded => 'true' });

        my $attribdescription     = "";
        $attribdescription        = $attr->{description} if exists $attr->{description};

        my $attribconstraints     = formatConstraints($dtoattributetype, $attr);
        $attribdescription        = formatDescription(' ' x 4, $attribdescription, $attribconstraints eq "") if $attribdescription ne "";

        my $constraintopening     = ($attribdescription ne "") ? ' * ' : '/**';
        $attribconstraints        = "    ${constraintopening}${attribconstraints}\n     */\n" if $attribconstraints ne "";

        my $xmlAnnotation         = "";

        SWITCH:
        {
            ##
            #   CAVEAT: intercept Map types before anything else, since they are not stored in the symbol table
            ##
            (exists $attr->{isMap}) && do
            {
                $dtoattributetype = SymbolTable::deriveMapTypeDeclaration($attr, {}, {'usejava' => 'true'});
                $dtoattributetype = "ArrayList<${dtoattributetype}>" if $isArray;

                if (MAIN::checkOption($configoptions, 'xmlSerialization'))
                {
                    $xmlAnnotation    = << "EOT";
    \@JacksonXmlProperty(localName = "${dtoattributename}")
    \@JacksonXmlElementWrapper(useWrapping = false)
EOT
                }
                last SWITCH;
            };
            ($isArray) && do
            {
                my $elementtype   = SymbolTable::resolveDatatypeName($attr);
                $elementtype      = $inbuilttypes->{$elementtype} if exists $inbuilttypes->{$elementtype};
                $dtoattributetype = "ArrayList<$elementtype>";

                if (MAIN::checkOption($configoptions, 'xmlSerialization'))
                {
                    $xmlAnnotation    = << "EOT";
    \@JacksonXmlProperty(localName = "${dtoattributename}")
    \@JacksonXmlElementWrapper(useWrapping = false)
EOT
                }
                last SWITCH;
            };
            (defined $enumclassinfo) && do
            {
                if (!exists $dereferencedAttribute->{isDereferenced})
                {
                    $enumclassdeclaration  =  $enumclassinfo->{classdefinition};
                    $enumclassdeclaration  =~ s/^/    /g;
                    $enumclassdeclaration  =~ s/\n/\n    /g;
                    $enumclassdeclaration  =~ s/\s+$//g;
                    $enumclassdeclaration .=  "\n\n";
                    $dtoattributetype      =  $enumclassinfo->{classname};
                }
                else
                {
                    $dtoattributetype = $attr->{name};
                }

                if (MAIN::checkOption($configoptions, 'xmlSerialization'))
                {
                    $xmlAnnotation = <<"EOT";
    \@JacksonXmlProperty(localName = "${dtoattributename}")
EOT
                }
                last SWITCH;
            };
            (exists $inbuilttypes->{$dtoattributetype}) && do
            {
                $dtoattributetype = $inbuilttypes->{$dtoattributetype};

                if (MAIN::checkOption($configoptions, 'xmlSerialization'))
                {
                    $xmlAnnotation = << "EOT";
    \@JacksonXmlProperty(localName = "${dtoattributename}")
EOT
                }
                last SWITCH;
            };
            do {
                if (!defined $dtoattributetype)
                {
                    die "could not derive attribute type for $dtoattributename";
                }
            };
        }

        my $dtoattribcardinality  = "";
        my $dtojsoncardinality    = "";

        if (MAIN::checkOption($configoptions, 'attribCardinality'))
        {
            $dtoattribcardinality = extractParamCardinality($attr, [ 'mandatory', 'optional ' ]);
            $dtoattribcardinality = "// $dtoattribcardinality";
        }

        if (MAIN::checkOption($configoptions, 'jsonCardinality'))
        {
            $dtojsoncardinality = extractJsonCardinality($attr, [ "true", "false" ]);
            $dtojsoncardinality = "(required = $dtojsoncardinality)";
        }

        my $dtotypedattribute        = "$dtoattributetype $lcdtoattributename";
        $dtoclasstypedattributelist  = "${dtoclasstypedattributelist}${ctadelim}${dtotypedattribute}";

        $dtoclassparmattributelist  .= "${ctadelim}${lcdtoattributename}";

        $ctadelim   = ', ';

        my $temp;

        my $padding = 70 - 5 - length(${dtotypedattribute});
        $padding    = 1 if $padding <= 0;
        $padding    = ' ' x $padding;
        $temp = <<"EOT";
${enumclassdeclaration}${attribdescription}${attribconstraints}    \@JsonProperty${dtojsoncardinality}
${xmlAnnotation}    \@JsonIgnore
    private ${dtotypedattribute};${padding}${dtoattribcardinality}

EOT
        $dtoclassajsonattributelist = "${dtoclassajsonattributelist}${temp}";

        $temp = <<"EOT";
        this.${lcdtoattributename} = ${lcdtoattributename};
EOT
        $dtoclassinitialisationlist = "${dtoclassinitialisationlist}${temp}";

        $temp = <<"EOT";
    \@JsonProperty(\"${dtoattributename}\")
    public ${dtoattributetype} get${ucdtoattributename}()
    {
        return ${lcdtoattributename};
    }

    \@JsonProperty(\"${dtoattributename}\")
    public void set${ucdtoattributename}(${dtoattributetype} ${lcdtoattributename})
    {
        this.${lcdtoattributename} = ${lcdtoattributename};
    }
EOT
        $gettersettermethods = "${gettersettermethods}\n${temp}";

        $temp = <<"EOT";
        ${tosconcat}"${tosdelim}${dtoattributename}=" + ${lcdtoattributename}
EOT
        $dtoclassattributestostring = "${dtoclassattributestostring}${temp}";
        $tosdelim  = ', ';
        $tosconcat = '+ ';
    }

    my $result = {
          dtoclassajsonattributelist     => $dtoclassajsonattributelist
        , dtoclasstypedattributelist     => $dtoclasstypedattributelist
        , dtoclassparmattributelist      => $dtoclassparmattributelist
        , dtoclassinitialisationlist     => $dtoclassinitialisationlist
        , gettersettermethods            => $gettersettermethods
        , dtoclassattributestostring     => $dtoclassattributestostring
    };

    ##
    #   recursively call generateClassAttributeClauses()
    #   to obtain parameter and attribute lists for the
    #   super class constructor.
    ##
    if (exists $classentry->{extendedbasetype})
    {
        my $extendedbasetype      = $classentry->{extendedbasetype};
        my $extendedbaseclassname = $extendedbasetype->{name};

        my $temp                  = generateClassAttributeClauses($extendedbaseclassname);

        $result->{xdtoclasstypedattributelist} = $temp->{dtoclasstypedattributelist};
        $result->{xdtoclassparmattributelist}  = $temp->{dtoclassparmattributelist};

        ##
        #   prefix the basetype attributes to those of the derived class
        #   so that the constructor arguments are properly aligned.
        ##

        if (exists $temp->{xdtoclasstypedattributelist})
        {
            $result->{xdtoclasstypedattributelist}  = $temp->{xdtoclasstypedattributelist} . ", " . $result->{xdtoclasstypedattributelist};
        }

        if (exists $temp->{xdtoclassparmattributelist})
        {
            $result->{xdtoclassparmattributelist}   = $temp->{xdtoclassparmattributelist} . ", " . $result->{xdtoclassparmattributelist};
        }

        $temp = "            super.toString()";

        if ($tosconcat ne "  ")
        {
            $temp = <<"EOT";
            super.toString() ${tosconcat}"${tosdelim}" ${tosconcat}
EOT
        }

        $result->{dtoclassattributestostring} = "${temp}${dtoclassattributestostring}";
    }

    return $result;
}

##
#   formatParameterList :
#
#   distribute parameters of multiple lines if the parameter line length exceeds a maximum
##
sub formatParameterList
{
    my ( $parameterline, $maxlength ) = @_;

    return $parameterline if length($parameterline) <= $maxlength;

    my @paramarray = split ', ', $parameterline;

    $parameterline = '';
    my $temp       = "\n" . ' ' x 6;
    my $delim      = '  ';
    for my $param (@paramarray)
    {
        if ( length($temp) + length($param) > $maxlength )
        {
            $parameterline .= "${temp}\n";
            $temp = ' ' x 6;
        }
        $temp .= "${delim}${param}";
        $delim = ", ";
    }

    $parameterline .= "${temp} ";

    return $parameterline;
}

##
#   generateModelClassDefinition :
#
#   generate a java class for a composite data type
##
sub generateModelClassDefinition
{
    my ($classname) = @_;

    my $modelpackage = $configoptions->{model}{package};
    my $outputpath   = $configoptions->{model}{path};

    die "INTERNAL ERROR : generateModelClassDefinition : $classname not found in symboltable" if !exists $symboltable->{$classname};

    my $classentry   = $symboltable->{$classname};

    # if ($classname eq "EndpointSettings")
    # {
    #     printf "found!\n";
    # }

    ##
    #   return if this is an alias entry or a dummy class
    ##
    return if exists $classentry->{reference} || exists $classentry->{isDummyClass};

    ##
    #  generate an enum class if it is an enum symbol table entry
    ##
    if (exists $classentry->{definition})
    {
        generateEnumClassDefinition($classname, $classentry, $classname, $modelpackage, { writetofile => 'true' });
        return;
    }

    ##
    #   ------------------  create the attribute-dependent sections  ------------------
    ##

    my $classattributes            = generateClassAttributeClauses($classname);
    my $dtoclassajsonattributelist = $classattributes->{dtoclassajsonattributelist};
    my $dtoclasstypedattributelist = $classattributes->{dtoclasstypedattributelist};
    my $dtoclassinitialisationlist = $classattributes->{dtoclassinitialisationlist};
    my $gettersettermethods        = $classattributes->{gettersettermethods};
    my $dtoclassattributestostring = $classattributes->{dtoclassattributestostring};

    ##
    #   ------------------  process extended class(es) ---------------------------------
    ##

    my $extendsclause         = '';
    my $initializesuperclause = '';
    if (exists $classentry->{extendedbasetype})
    {
        die "xdtoclasstypedattributelist missing for $classname" if !exists $classattributes->{xdtoclasstypedattributelist};
        die "xdtoclassparmattributelist missing for $classname"  if !exists $classattributes->{xdtoclassparmattributelist};

        my $extendedbasetype            = $classentry->{extendedbasetype};
        my $extendedbaseclassname       = $extendedbasetype->{name};
        $extendsclause                  = " extends ${extendedbaseclassname}";

        ##
        #   concatenate base type and derived type attribute lists
        ##
        my $xdtoclasstypedattributelist = $classattributes->{xdtoclasstypedattributelist};

        $dtoclasstypedattributelist     = ($dtoclasstypedattributelist ne "")
                                          ? "${xdtoclasstypedattributelist}, ${dtoclasstypedattributelist}"
                                          : "${xdtoclasstypedattributelist}";

        my $xdtoclassparmattributelist  = $classattributes->{xdtoclassparmattributelist};

        $xdtoclassparmattributelist     = formatParameterList($xdtoclassparmattributelist, 108);

        $initializesuperclause          = <<"EOT";
        super($xdtoclassparmattributelist);

EOT
    }

    ##
    #   ------------------  generate import statements  --------------------------
    ##

    my $dtoclass = $classname;
    my $dtoclassimports = "";

    if (exists $classentry->{classimports})
    {
        for my $import (sort keys %{$classentry->{classimports}})
        {
            my $temp = <<"EOT";
import ${modelpackage}.${import};
EOT
            $dtoclassimports .= $temp;
        }
    }

    $dtoclassimports .= "\n" if $dtoclassimports ne "";

    ##
    #   ------------------  generate a sample class instantiation  ---------------------------
    ##

    my $dtoclassinstance = "";
    if (MAIN::checkOption($configoptions, 'generateClient') && exists $classentry->{exposed})
    {
        $dtoclassinstance = generateModelClassInstance(0, ' ', 'payload', $modelpackage, $classname, {}, 'mandatory');
    }

    ##
    #   ------------------  assemble the sections into a class  ------------------
    ##

    my $classdescription = "";
    $classdescription    = formatDescription('', $classentry->{description}, 1) if exists $classentry->{description};
    if ($classdescription eq "" && exists $classentry->{exposed})
    {
        $classdescription = "DESCRIPTION:\n" . $inputhash->{info}{description} if exists $inputhash->{info} && exists $inputhash->{info}{description};
        $classdescription = formatDescription('', $classdescription, 1) if defined $classdescription && $classdescription ne "";
    }

    if ($classentry->{composed})
    {
        $classdescription .= sprintf "\n/* COMPOSED: %s */\n\n", $classentry->{composed};
    }

    my $jsonconfiguration = "";
    $jsonconfiguration = "\@JsonInclude(JsonInclude.Include.NON_NULL)" if MAIN::checkOption($configoptions, 'jsonIncludeNonNull');

    my $xmlconfiguration = "";
    my $xmlimportclasses = "";
    if ( MAIN::checkOption($configoptions, 'xmlSerialization') )
    {
        $xmlimportclasses = << "EOT";

import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;
EOT
        $xmlconfiguration = "\n\@JacksonXmlRootElement";
    }

    $dtoclasstypedattributelist = formatParameterList($dtoclasstypedattributelist, 108);

    my $completeclass = <<"EOT";
package ${modelpackage};

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;${xmlimportclasses}
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;

${dtoclassimports}${dtoclassinstance}${classdescription}${jsonconfiguration}${xmlconfiguration}
public class ${dtoclass}${extendsclause}
{
${dtoclassajsonattributelist}
    // default constructor for use in jackson deserialization
    public ${dtoclass}()
    {
        super();
    }

    // parameterized constructor
    public ${dtoclass}( ${dtoclasstypedattributelist} )
    {
${initializesuperclause}${dtoclassinitialisationlist}    }
${gettersettermethods}
    \@Override
    public String toString()
    {
        return "${dtoclass} [" +
${dtoclassattributestostring}        + "]";
    }
}
EOT

    FileInterface::writeModelClass($outputpath, $dtoclass, $completeclass);
}

########################################################################
#       Generate Service class
########################################################################

##
#   generateServiceClassHeader :
#
#   generate a stub class header
##
sub generateServiceClassHeader
{
    my ($modelpackage, $servicepackage, $classname, $classimports) = @_;

    my $dtoclassimports = "";
    my $temp;
    for my $import (sort keys %$classimports)
    {
        $import = "${modelpackage}.${import}";
        $temp   = <<"EOT";
import ${import};
EOT
        $dtoclassimports .= $temp;
    }

    my $basepackage         = $configoptions->{basepackage};

    my $errorresponseclause = "";
    if (MAIN::checkOption($configoptions, 'useErrorResponse'))
    {
        $errorresponseclause = <<"EOT";

import ${basepackage}.exception.ErrorDetails;
import ${basepackage}.exception.ErrorResponse;
EOT
    }

    my $classheader = <<"EOT";
package ${servicepackage};

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;

import javax.servlet.http.HttpServletRequest;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Maps;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
//import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

${dtoclassimports}
import ${basepackage}.constants.CommonConstants;
import ${basepackage}.constants.ExceptionConstants;
import ${basepackage}.exception.TechnicalException;
import ${basepackage}.service.utils.ServiceUtil;
${errorresponseclause}
\@org.springframework.stereotype.Service
public class ${classname}Impl implements ${classname}
{
EOT

    return { header => $classheader };
}

##
#   generateServiceClassMethod :
#
#   generate a stub class method
##
sub generateServiceClassMethod
{
    my ($si, $modelpackage, $classname, $classimports) = @_;

    my $endpoint               = $si->{endpoint};
    my $requestmethod          = $si->{method};
    my $servicemethodname      = $si->{servicemethodname};
    my $invocationParameters   = $si->{parameters};
    my $parameterlist          = $si->{parameterlist};

    my $ucMethod = ucfirst Parser::formatCamelCase($servicemethodname, "[^A-Za-z0-9]+");

    $invocationParameters      = "\n${invocationParameters} " if $invocationParameters ne "";

    my $invocationResponseCode = 200;
    $invocationResponseCode    = $si->{responsecode} if exists $si->{responsecode} && $si->{responsecode} ne "";
    my $invocationResponseType = $si->{responsetype} if exists $si->{responsetype} && $si->{responsetype} ne "";
    my $responseIsArray        = $si->{responseIsArray};
    my $responseIsInbuilt      = $si->{responseIsInbuilt};

    ##
    #   map the numeric response code to an enum value from HttpStatus
    ##
    if (exists $mapResponseCodeToEnum->{$invocationResponseCode})
    {
        $invocationResponseCode = $mapResponseCodeToEnum->{$invocationResponseCode};
        $invocationResponseCode = "HttpStatus.${invocationResponseCode}";
    }

    ##
    #   ------------------  create response instantiation code  ------------------
    ##

    my $responseClassInstance  = '    Void response = null;';

    if (defined $invocationResponseType && $responseIsInbuilt ne 'true')
    {
        $responseClassInstance = generateModelClassInstance(0, ' ', 'payload', $modelpackage, $invocationResponseType, $classimports, 'mandatory');
        $responseClassInstance =~ s/\nimport [^;]+;//g;
        $responseClassInstance =~ s/(\/\*\n)|(\n\*\/)//g;
        $responseClassInstance =~ s/\n+$//g;
        $responseClassInstance =~ s/request =/response=/;
        $responseClassInstance =~ s/\n/\n        /g;
    }
    else
    {
        $responseClassInstance = generateModelInbuiltInstance(0, '', "response", $invocationResponseType, undef);
        $responseClassInstance = "${invocationResponseType} response = ${responseClassInstance};"
    }

    ##
    #   ------------------  assemble the sections into a method ------------------
    ##

    my $parameterlistclause = "";
    if (scalar @$parameterlist > 0)
    {
        my $indent = " " x 5;
        for my $param (@$parameterlist)
        {
            $parameterlistclause .= $indent . " *  \@param " . $param . "\n";
        }
        $parameterlistclause .= $indent . " *  \@param httpservletrequest";
    }

    my $responseentityclause    = "ResponseEntity<Void>";
    $responseentityclause       = "ResponseEntity<$invocationResponseType>" if defined $invocationResponseType;

    my $responseVariableName    = "response";
    my $responseListAllocation  = "";

    if (defined $invocationResponseType && $responseIsArray eq 'true')
    {
        $responseentityclause   = "ResponseEntity<List<$invocationResponseType>>";
        $responseVariableName   = "listResponse";
        $responseListAllocation = <<"EOT";

        ArrayList<$invocationResponseType> listResponse =
        new ArrayList<$invocationResponseType> (
            Arrays.asList(
                response
            )
        );
EOT
    }

    my $completemethod = <<"EOT";

    /** ${requestmethod} : ${endpoint}
      *
${parameterlistclause}
      *  \@return
      *  \@throws TechnicalException
      */
    public ${responseentityclause} invoke${ucMethod}(${invocationParameters})  throws TechnicalException
    {
        HttpHeaders header = null;

        header = serviceutil.getHeaders( httpservletrequest, null );
    	header.add("IDEMPOTENCY_ID", httpservletrequest.getHeader("PROCESS_ID"));

    	return invoke${ucMethod}Service( httpservletrequest );
    }

    /**
     *  Implement service call for invoke${ucMethod}
     *
     *  \@param httpservletrequest
     *  \@return
     *  \@throws TechnicalException
     */
    private ${responseentityclause} invoke${ucMethod}Service( HttpServletRequest httpservletrequest ) throws TechnicalException
    {
        ResponseEntity<String> responseObj = new ResponseEntity<String>( "dummy response", null, ${invocationResponseCode});

        // TODO: implement method body of service invocation

        return populate${ucMethod}Response( responseObj, $invocationResponseCode );
    }

    /**
     *  Return response object for invoke${ucMethod}
     *
     *  \@param httpResponse
     *  \@param expectedHttpStatus
     *  \@return
     *  \@throws TechnicalException
     */
    private ${responseentityclause} populate${ucMethod}Response( ResponseEntity<String> httpResponse, HttpStatus expectedHttpStatus ) throws TechnicalException
    {
        interceptEmptyResponse(httpResponse, "ERR001", "[${requestmethod}:${endpoint}] populate${ucMethod}Response: empty response" );
        interceptHttpErrorCode(httpResponse.toString(), "ERR002", "[${requestmethod}:${endpoint}] populate${ucMethod}Response", expectedHttpStatus, httpResponse.getStatusCode() );

        ${responseClassInstance}
${responseListAllocation}
        return new ${responseentityclause}( ${responseVariableName}, null, $invocationResponseCode );
    }
EOT

    my $interface = <<"EOT";

    /** ${requestmethod} : ${endpoint}
     */
    public ${responseentityclause} invoke${ucMethod}(${invocationParameters}) throws TechnicalException;
EOT

    return { code => $completemethod, interface => $interface };
}

##
#   generateServiceClass :
#
#   generate a service stub class implementing
##
sub generateServiceClass
{
    my ($svcinfo) = @_;

    my $modelpackage   = $configoptions->{model}{package};
    my $servicepackage = $configoptions->{service}{package};
    my $outputpath     = $configoptions->{service}{path};

    my $classmethods   = "";
    my $interfaces     = "";

    my $classname      = ucfirst Parser::addCamelSuffix($svcinfo->{classname}, "Service");
    if (!MAIN::checkOption($configoptions, 'clusterByTag'))
    {
        $classname = $configoptions->{classnames}{service} if exists $configoptions->{classnames}{service};
    }
    else
    {
        die "INTERNAL ERROR: \$tag undefined while using 'clusterByTag'" if !exists $svcinfo->{tag};
        $classname = ucfirst Parser::addCamelSuffix($svcinfo->{tag}, "Service");
    }

    my $dtoclassimports = $svcinfo->{imports};

    for my $si (@{$svcinfo->{serviceinfo}})
    {
        my $method     = generateServiceClassMethod($si, $modelpackage, $classname, $dtoclassimports);
        $classmethods .= $method->{code};
        $interfaces   .= $method->{interface};
    }

    if ($configoptions->{outputstate} == 0)
    {
        # collect information in an intermediate array and return if this is not the last swagger input file

        push @{$collectedClassInformation->{service}}, { methods => $classmethods, interfaces => $interfaces, imports => $dtoclassimports };
        return;
    }

    # merge the information from the intermediate array into sections and write those to the output file

    if (exists $collectedClassInformation->{service})
    {
        push @{$collectedClassInformation->{service}}, { methods => $classmethods, interfaces => $interfaces, imports => $dtoclassimports };

        $dtoclassimports = {};
        $classmethods    = "";
        $interfaces      = "";

        for my $s (@{$collectedClassInformation->{service}})
        {
            die 'interfaces missing' if !exists $s->{interfaces};
            die 'methods missing'    if !exists $s->{methods};

            if (exists $s->{imports})
            {
                map {$dtoclassimports->{$_} = 1;} keys %{$s->{imports}};
            }

            $interfaces   .= $s->{interfaces};
            $classmethods .= $s->{methods};
        }

        die 'service class name missing' if !exists $configoptions->{classnames}{service};
        $classname = $configoptions->{classnames}{service}
    }

    my $classheader  = generateServiceClassHeader($modelpackage, $servicepackage, $classname, $dtoclassimports);
    my $headertext   = $classheader->{header};

    my $serviceclass = <<"EOT";
$headertext
    private static final Logger logger = LoggerFactory.getLogger(${classname}Impl.class);

    \@Autowired
    ServiceUtil serviceutil;

    /**
      * use ResponseEntity<?> with a wildcard so that the parameter will accept all ResponseEntity subtypes
      */
    private void interceptEmptyResponse( ResponseEntity<?> httpResponse, String errorcode, String errmsg ) throws TechnicalException
    {
        if ( !ObjectUtils.isEmpty(httpResponse))
            return;

        ErrorResponse response = new ErrorResponse(
                new ErrorDetails(
                     errorcode
                    ,errmsg
                )
        );

        throw new TechnicalException( response );
    }

    private void interceptHttpErrorCode( String responsebody, String errorcode, String errmsg, HttpStatus expectedHttpStatus, HttpStatus actualHttpStatus ) throws TechnicalException
    {
        if (expectedHttpStatus.value() == actualHttpStatus.value())
            return;

        if (errmsg != null)
            errmsg = String.format("%s. ", errmsg);
        else
            errmsg = "";

        String errorResponse = String.format("%sMismatch of actual HTTP Response Code=%s vs. expected=%s. Response body : %s", errmsg, actualHttpStatus.toString(), expectedHttpStatus.toString(), responsebody );
        logger.error(CommonConstants.ERROR_IN_REGIONAL_CALL, errorResponse);

        ErrorResponse response = new ErrorResponse(
                new ErrorDetails(
                         errorcode
                        ,errorResponse
                )
        );

        throw new TechnicalException( response );
    }

$classmethods
}
EOT

    my $interfaceclass = <<"EOT";
$headertext
$interfaces
}
EOT

    $interfaceclass =~ s/ServiceImpl implements [^ ]+{/Service\n{/g;
    $interfaceclass =~ s/public class/public interface/;

    if ($outputpath eq '-')
    {
        print $interfaceclass;
        print $serviceclass;
    }
    else
    {
        printf "output : %s\n", "${classname}.java" if MAIN::checkOption($configoptions, 'verbose', 'flags');
        FileInterface::writeFile($outputpath, "${classname}.java", $interfaceclass);

        printf "output : %s\n", "${classname}Impl.java" if MAIN::checkOption($configoptions, 'verbose', 'flags');
        FileInterface::writeFile($outputpath, "${classname}Impl.java", $serviceclass);
    }
}

########################################################################
#       Generate Controller class
########################################################################

##
#   generateControllerClassHeader :
#
#   generate a class header
##
sub generateControllerClassHeader
{
    my ($configoptions, $controllerclassname, $tag, $classimports, $basepath, $title, $classdescription) = @_;

    my $modelpackage      = $configoptions->{model}{package};
    my $servicepackage    = $configoptions->{service}{package};
    my $controllerpackage = $configoptions->{controller}{package};

    my $dtoclassimports   = "";
    my $temp;
    for my $import (sort keys %$classimports)
    {
        $import = "${modelpackage}.${import}" if $import !~ /^java/;
        $temp = <<"EOT";
import ${import};
EOT
        $dtoclassimports .= $temp;
    }

    my $servicetypename;

    if (!MAIN::checkOption($configoptions, 'clusterByTag'))
    {
        $servicetypename = $configoptions->{classnames}{service} if exists $configoptions->{classnames}{service};
    }
    else
    {
        die "INTERNAL ERROR: \$tag undefined while using 'clusterByTag'" if !defined $tag;
        $servicetypename = ucfirst Parser::addCamelSuffix($tag, "Service");
    }

    my $serviceinstancename = lcfirst $servicetypename;

    $classdescription       = "" if !defined $classdescription;

    my $titleclause         = "";
    $titleclause            = "\@Api(value = \"${title}\")" if defined $title;
    my $basepathclause      = "";
    $basepathclause         = "\@RequestMapping(\"${basepath}\")" if defined $basepath;

    my $basepackage         = $configoptions->{basepackage};

    my $errorresponseclause = "";
    if (MAIN::checkOption($configoptions, 'useErrorResponse'))
    {
        $errorresponseclause = <<"EOT";

import ${basepackage}.exception.ErrorDetails;
import ${basepackage}.exception.ErrorResponse;
EOT
    }

    my $classheader = <<"EOT";
${classdescription}
package ${controllerpackage};

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import ${basepackage}.exception.TechnicalException;
${errorresponseclause}
${dtoclassimports}
import ${servicepackage}.${servicetypename};

\@RestController
${basepathclause}
${titleclause}
public class ${controllerclassname}
{
    \@Autowired
    private ${servicetypename} ${serviceinstancename};

EOT

    return { header => $classheader };
}

##
#   generateControllerClassParameterClauses :
#
#   generate a class method
##
sub generateControllerClassParameterClauses
{
    my ($endpoint, $method, $parameters, $dtoclassimports) = @_;

    my $parameterSection     = "";
    my $serviceParameters    = "";
    my $parameterlist        = [];
    my $parameterhash        = {};
    my $spLineLength         = 0;
    my $invocationParameters = "";
    my $bodyIsProcessed      = 0;

    # if ($endpoint eq "/access_settings/whitelist/{rule_id}")
    # {
    #     printf "found!\n";
    # }

    for my $p (@$parameters)
    {
        if (!exists $p->{in})
        {
            die "reference expected while missing {in} : ${method}::${endpoint}" if !exists $p->{'$ref'};
            $p = SwaggerUtils::resolveDataTypeReference($p);
            $p = $p->{type};
        }

        ##
        #   skip body parameters that have been merged into a compound type
        #   by 'processEndPointParameters'.
        ##
        next if exists $p->{isMerged};

        my $name   = $p->{name}                   if exists $p->{name};
        my $lcname = generateParameterName($name) if defined $name;
        ##
        #   prefix the generated name with an underscore to
        #   prevent potential collisions within the java namespace.
        ##
        $lcname    = "_${lcname}";

        if (exists $parameterhash->{$lcname})
        {
            my $filename = 'swagger file';
            $filename = basename($configoptions->{inputfile}) if exists $configoptions->{inputfile};

            warn "[$filename] ${method}::${endpoint} : parameter $lcname listed at least twice";

            ##
            #   make the parameter name unique
            ##
            while (exists $parameterhash->{$lcname})
            {
                $lcname .= 'x';
            }
        }

        $parameterhash->{$lcname} = 1;

        my $description     = $p->{description} if exists $p->{description};

        if (!exists $p->{classdefinition})
        {
            die "parameter is missing class definition"
        }

        my $classdefinition = $p->{classdefinition};

        if (exists $classdefinition->{reference} && SymbolTable::resolveSymboltableReference($classdefinition)->{isDummyClass})
        {
            my $filename = 'swagger file';
            $filename = basename($configoptions->{inputfile}) if exists $configoptions->{inputfile};

            warn "[$filename] dummy class reference in ${method}::${endpoint} : parameter $lcname ";
        }

        ##
        #   suppress parameters that are meant to add an OAuth2 token to a service signature.
        #   these are consumed by the API Manager and should be removed from the swagger file.
        ##
        next if MAIN::checkOption($configoptions, 'skipAuthorization') && ($name =~ m/Authorization/i && defined $description && $description =~ m/Token/i);

        my $cardinality = extractParamCardinality($p, [ 'true', 'false' ]);
        my $type        = SymbolTable::resolveSymbolAttribTypeName($classdefinition);

        ##
        #   suppress parameters that are typed 'null'.
        #   some swagger files specify them to mark the absence of a request body.
        ##
        next if $type eq 'null';

        if (!defined $type || $type eq 'object')
        {
            $type = Parser::addCamelSuffix(Parser::generateTypePrefix("${endpoint}-${method}"), ${name});
            die "synthetic type $type not found" if !exists $symboltable->{$type};
        }

        my $isInbuilt = 0;
        if (exists $inbuilttypes->{$type})
        {
            $type = $inbuilttypes->{$type};
            $isInbuilt = 1;
            ##
            #   map binary input streams to 'byte []' instead of 'String'
            ##
            if (exists $classdefinition->{isBinary})
            {
                $type = 'byte []';
            }
        }

        my $isArray = 0;
        if (exists $classdefinition->{type} && $classdefinition->{type} eq 'array')
        {
            my $elementtype = SymbolTable::resolveDatatypeName($classdefinition);

            my $type = $elementtype;

            if (exists $inbuilttypes->{$type})
            {
                $isInbuilt = 1;
                $type = $inbuilttypes->{$type};
            }

            $isArray = 1;
        }

        my $controllerParameter = "";
        SWITCH:
        {
            ($p->{in} =~ m/header/i) && do {
                $controllerParameter   = sprintf "        \@RequestHeader(value = \"%s\", required = %s) String %s,", $name, $cardinality, $lcname;
                $invocationParameters .= "${invocationPad}String $lcname,\n";
                last SWITCH;
            };
            ($p->{in} =~ m/path/i) && do {
                $dtoclassimports->{$type} = 1 if !$isInbuilt;
                $controllerParameter   = sprintf "        \@PathVariable(value = \"%s\", required = %s) %s %s,", $name, $cardinality, $type, $lcname;
                $invocationParameters .= "${invocationPad}${type} $lcname,\n";
                last SWITCH;
            };
            ($p->{in} =~ m/body/i) && do {
                ##
                #	multiple in:body parameters are merged into a single synthetic type.
                #	We have to be careful not to generate method parameters for the merged parameters.
                ##
                if (!$bodyIsProcessed)
                {
                    $dtoclassimports->{$type} = 1 if !$isInbuilt;
                    $type = "ArrayList<${type}>" if $isArray;
                    $controllerParameter   = sprintf "        \@RequestBody %s %s,", $type, $lcname;
                    $invocationParameters .= "${invocationPad}${type} $lcname,\n";
                    $bodyIsProcessed = 1;
                }
                last SWITCH;
            };
            ($p->{in} =~ m/query/i) && do {
                $dtoclassimports->{$type} = 1 if !$isInbuilt;
                $type                  = "ArrayList<${type}>" if $isArray;
                $controllerParameter   = sprintf "        \@RequestParam(value = \"%s\", required = %s) %s %s,", $name, $cardinality, $type, $lcname;
                $invocationParameters .= "${invocationPad}${type} $lcname,\n";
                last SWITCH;
            };
            ($p->{in} =~ m/formData/i) && do {
                $dtoclassimports->{$type} = 1 if !$isInbuilt;
                $controllerParameter   = sprintf "        \@FormData(\"%s\") %s %s,", $name, $type, $lcname;
                $invocationParameters .= "${invocationPad}${type} $lcname,\n";
                last SWITCH;
            };
            do {
                die "unsupported parameter location $p->{in}";
            };
        }

        push @$parameterlist, $lcname;

        if (defined $description)
        {
            my $pad = 92 - length($controllerParameter);
            $pad = 1 if $pad < 0;
            $controllerParameter = $controllerParameter . sprintf "%s// %s", ' ' x $pad, formatDescriptionAsOneLine($description, 72);
        }

        $parameterSection  .= "${controllerParameter}\n";
        $serviceParameters .= ', ' if $serviceParameters ne "";
        if ($spLineLength > 80)
        {
            $serviceParameters .= "\n                   ";
            $spLineLength       = 0;
        }

        $serviceParameters .= $lcname;
        $spLineLength      += length($lcname) + 2;
    }

    return { section           => $parameterSection
        , parameters           => $serviceParameters
        , parameterlist        => $parameterlist
        , invocationparameters => $invocationParameters };
}

##
#   generateControllerClassResponseClauses :
#
#   generate a class method
##
sub generateControllerClassResponseClauses
{
    my ($endpoint, $method, $responses, $dtoclassimports) = @_;

    my $responseSection        = "";
    my $invocationResponseCode = "";
    my $invocationResponseType = "";
    my $containerClause        = "";
    my $responseIsArray        = undef;
    my $responseIsInbuilt      = undef;

    my @sortedResponseCodes    = Parser::sortResponseCodes($responses);

    for my $rc (@sortedResponseCodes)
    {
        my $response           = $responses->{$rc};
        $containerClause       = "";
        my $messageclause      = '';
        if (exists $response->{description})
        {
            my $description = sanitizeDescription($response->{description});
            $messageclause  = sprintf ", message = \"%s\"", $description if defined $description;
        }
        $messageclause         = ", message = \"\"" if $messageclause eq '';

        my $responsetypeclause = "";
        my $type               = "";
        my $isInbuilt          = 0;

        if (!exists $response->{classdefinition})
        {
            die "response is missing class definition";
        }

        my $classdefinition = $response->{classdefinition};

        SWITCH:
        {
            (exists $classdefinition->{definition}) && do
            {
                my $definition   = $classdefinition->{definition};
                my $responsetype = $definition->{type};
                $type            = $inbuilttypes->{$responsetype};
                $isInbuilt       = 1;
                last SWITCH;
            };
            (exists $classdefinition->{reference} && !exists $classdefinition->{type}) && do
            {
                $type = $classdefinition->{reference};
                ##
                #   map dummy classes to void response types.
                ##
                if (SymbolTable::resolveSymboltableReference($classdefinition)->{isDummyClass})
                {
                    my $filename = 'swagger file';
                    $filename    = basename($configoptions->{inputfile}) if exists $configoptions->{inputfile};

                    warn "[$filename] dummy class reference in ${method}::${endpoint} : response $rc";

                    $type        = 'Void';
                    $isInbuilt   = 1;
                }
                last SWITCH;
            };
            (exists $classdefinition->{type} && $classdefinition->{type} eq 'array') && do
            {
                my $elementtype  = SymbolTable::resolveDatatypeName($classdefinition);

                die "childtype undefined for rc $rc" if !defined $elementtype;

                $type            = $elementtype;

                if (!MAIN::checkOption($configoptions, 'useArrayResponseType'))
                {
                    $containerClause = ", responseContainer = \"List\"";
                    $responseIsArray = 'true';
                }
                else
                {
                    $type        = "ArrayOf${elementtype}";

                    if (!exists $symboltable->{$type})
                    {
                        my $arrayOfType = {
                            name           => $type
                          , attriblist     => [
                                {
                                     name     => 'arrayOf'
                                   , required => 'true'
                                   , type     => {
                                         type  => 'array'
                                       , items => {
                                             reference => $elementtype
                                         }
                                     }
                                }
                            ]
                          , classimports   => { $elementtype => 1 }
                          , exposed        => 1
                          , processed      => 1
                          , filename       => $configoptions->{inputfile}
                        };

                        SymbolTable::createSymboltableEntry($arrayOfType);
                    }
                }
                last SWITCH;
            };
            do {
                die "unsupported response type";
            }
        }

        $responsetypeclause       = ", response = ${type}.class" if $type ne "Void" && $type ne 'Null';
        $dtoclassimports->{$type} = 1                            if !$isInbuilt && !isClassImportSuppressed($type);

        if ($rc =~ m/[12][0-9][0-9]/ && $invocationResponseType eq "" && $type ne "")
        {
            $invocationResponseCode = $rc;
            $invocationResponseType = $type;
            $responseIsInbuilt      = 'true' if $isInbuilt;
        }

        my $temp = <<"EOT";
            \@ApiResponse(code = ${rc}${messageclause}${responsetypeclause}${containerClause}),
EOT
        $responseSection .= $temp;
    }

    my $result = { section => $responseSection, code => $invocationResponseCode, type => $invocationResponseType };
    $result->{isArray}   = 'true' if defined $responseIsArray;
    $result->{isInbuilt} = 'true' if defined $responseIsInbuilt;

    return $result;
}

##
#   deriveControllerClassname
#
#   derive a unique classname for the controller class
##
sub deriveControllerClassname
{
    # extract the classname for the controller class either from the metainformation
    # in the swaggerfile or from user input on the command line.

    my $classname = $inputhash->{tags}[0]{name}              if exists $inputhash->{tags} && exists $inputhash->{tags}[0]{name};
    $classname    = $inputhash->{info}{title}                if !defined $classname       && exists $inputhash->{info} && exists $inputhash->{info}{title};
    $classname    = $configoptions->{classnames}{controller} if exists $configoptions->{classnames}{controller};

    die "controllerclass classname undefined" if !defined $classname;

    # remove all non-alpha characters from the classname
    $classname    =~ s/[^A-Za-z]//g;
    $classname    = ucfirst($classname);

    return $classname;
}

##
#   deriveControllerMethodName
#
#   derive a unique method name for the controller (and service) classes
#
#   CAVEAT: mixing operationId and endpoint/method derived algorithms is no good,
#           since the operationId is outside our control and can cause collisions
#           with the 2nd procedure.
##
sub deriveControllerMethodName
{
    my ($endpoint, $method) = @_;

    my $controllermethodname = lcfirst Parser::generateTypePrefix("${method}-${endpoint}");

    if (exists $collectedClassMethodIndex->{$controllermethodname})
    {
        my $filename1 = 'swagger file';
        $filename1 = basename($configoptions->{inputfile}) if exists $configoptions->{inputfile};

        my $filename2 = $collectedClassMethodIndex->{$controllermethodname}{inputfile};

        die "controller method $controllermethodname for $method::$endpoint derived from files $filename1 and $filename2";
    }

    return $controllermethodname
}

##
#   generateControllerClassMethod :
#
#   generate a class method exposing the 'parameters' and 'responses' interface from the swagger definition
##
sub generateControllerClassMethod
{
    my ($endpoint, $method, $dtoclassimports, $classname, $tag) = @_;

    my $paths = $inputhash->{paths};
    die "path: ${method}::${endpoint} not found" if !exists $paths->{$endpoint} || !exists $paths->{$endpoint}{$method};

    my $pi = $paths->{$endpoint};
    my $mi = $pi->{$method};

    # if ( $endpoint eq "/suppression/invalid_emails" )
    # {
    #     printf "found!\n";
    # }

    my $parameters = [];
    map {push @$parameters, $_} @{$pi->{parameters}} if exists $pi->{parameters};
    map {push @$parameters, $_} @{$mi->{parameters}} if exists $mi->{parameters};

    my $summary                = $mi->{summary}      if exists $mi->{summary};
    my $description            = $mi->{description}  if exists $mi->{description};
    my $consumes               = $mi->{consumes}     if exists $mi->{consumes};
    my $produces               = $mi->{produces}     if exists $mi->{produces};
    my $responses              = $mi->{responses}    if exists $mi->{responses};
    my $tags                   = $mi->{tags}         if exists $mi->{tags};

    ##
    #   ------------------  generate 'parameters' section ------------------------
    ##

    my $parameterSection       = "";
    my $serviceParameters      = "";
    my $invocationParameters   = "";
    my $parameterlist          = [];

    if (defined $parameters)
    {
        my $parameterClauses = generateControllerClassParameterClauses($endpoint, $method, $parameters, $dtoclassimports);

        $parameterSection     = $parameterClauses->{section};
        $serviceParameters    = $parameterClauses->{parameters};
        $invocationParameters = $parameterClauses->{invocationparameters};
        $parameterlist        = $parameterClauses->{parameterlist};

        $parameterSection     = "\n${parameterSection}       " if $parameterSection ne "";
    }

    ##
    #   always add the servlet request to the parameter list
    ##
    $parameterSection         .= " HttpServletRequest httpservletrequest ";
    $serviceParameters        .= ', ' if $serviceParameters ne "";
    $serviceParameters        .= 'httpservletrequest';
    $invocationParameters     .= "${invocationPad}HttpServletRequest httpservletrequest";

    ##
    #   ------------------  generate 'responses' section -------------------------
    ##

    my $responseSection        = "";
    my $invocationResponseCode = "";
    my $invocationResponseType = "";
    my $responseclause         = "";
    my $responseIsArray        = 'false';
    my $responseIsInbuilt      = 'false';

    if (defined $responses)
    {
        my $responseClauses     = generateControllerClassResponseClauses($endpoint, $method, $responses, $dtoclassimports);
        $responseSection        = $responseClauses->{section};
        $invocationResponseCode = $responseClauses->{code};
        $invocationResponseType = $responseClauses->{type};
        $responseIsArray        = $responseClauses->{isArray}   if exists $responseClauses->{isArray};
        $responseIsInbuilt      = $responseClauses->{isInbuilt} if exists $responseClauses->{isInbuilt};

        $responseclause = ", response = $invocationResponseType.class";
    }

    ##
    #   ------------------  assemble the sections into a method ------------------
    ##

    my $controllermethodname = deriveControllerMethodName($endpoint, $method);

    $description             = sanitizeDescription($description);
    $summary                 = sanitizeDescription($summary);

    my $valueclause          = "value=\"\"";
    $valueclause             = "value=\"$summary\"" if defined $summary;
    my $notesclause          = ", notes=\"\"";
    $notesclause             = ", notes=\"$description\"" if defined $description;

    my $tagsclause           = "";
    if (defined $tags)
    {
        my $tagsstring       = Parser::array2ConcatenatedStrings($tags);
        $tagsclause          = ", tags = { $tagsstring }"
    }

    my $producesclause       = "";
    if (defined $produces)
    {
        my $xmlContent       = "application/xml" if MAIN::checkOption($configoptions, 'xmlContent');
        my $producesstring   = Parser::array2ConcatenatedStrings($produces, {additionalentry=>$xmlContent});
        $producesclause      = "            ,produces = { $producesstring }\n"
    }

    my $consumesclause       = "";
    if (defined $consumes)
    {
        my $xmlAccept        = "application/xml" if MAIN::checkOption($configoptions, 'xmlAccept');
        my $consumesstring   = Parser::array2ConcatenatedStrings($consumes, {additionalentry=>$xmlAccept});
        $consumesclause      = "            ,consumes = { $consumesstring }\n"
    }

    my $responsetypeclause   = $invocationResponseType;
    if ($responseIsArray eq 'true')
    {
        $responsetypeclause  = "List<$invocationResponseType>";
    }

    # don't split an empty parameter list over 2 lines
    $parameterSection        =~ s/^\n$//g;

    my $basepathcomment      = "";
    $basepathcomment         = "\n    // Original basepath : " . $inputhash->{basePath} if exists $inputhash->{basePath};

    my $ucMethod = uc $method;

    my $servicetypename      = ucfirst Parser::addCamelSuffix($classname, "Service");
    if (!MAIN::checkOption($configoptions, 'clusterByTag'))
    {
        $servicetypename     = $configoptions->{classnames}{service} if exists $configoptions->{classnames}{service};
    }
    else
    {
        die "INTERNAL ERROR: \$tag undefined while using 'clusterByTag'" if !defined $tag;
        $servicetypename = ucfirst Parser::addCamelSuffix($tag, "Service");
    }

    my $serviceinstancename  = lcfirst $servicetypename;
    my $servicemethodname    = ucfirst $controllermethodname;

    my $completemethod       = <<"EOT";
    \@ApiOperation(${valueclause}${notesclause}${responseclause}${tagsclause})
    \@ApiResponses(value = {
${responseSection}    })${basepathcomment}
    \@RequestMapping(value = "${endpoint}"
${producesclause}${consumesclause}            ,method = RequestMethod.${ucMethod})
    public ResponseEntity<$responsetypeclause> ${controllermethodname}(${parameterSection}) throws TechnicalException
    {
        return ${serviceinstancename}.invoke${servicemethodname}(
                   ${serviceParameters});
    }

EOT

    my $filename = 'swagger file';
    $filename = basename($configoptions->{inputfile}) if exists $configoptions->{inputfile};

    my $result =
        {
            implementation => $completemethod,
            invocation     =>
            {
                inputfile            => $filename,
                endpoint             => $endpoint,
                method               => $method,
                controllermethodname => $controllermethodname,
                servicemethodname    => $servicemethodname,
                parameters           => $invocationParameters,
                parameterlist        => $parameterlist,
                responsecode         => $invocationResponseCode,
                responsetype         => $invocationResponseType,
                responseIsArray      => $responseIsArray,
                responseIsInbuilt    => $responseIsInbuilt
            }
    };

    $collectedClassMethodIndex->{$controllermethodname} = $result;

    return $result;
}

##
#   generateControllerAdviceClass :
#
#   generate a controller class exposing all endpoints and methods from the swagger file(s)
##
sub generateControllerAdviceClass
{
    my ($classname, $outputpath) = @_;

    my $controllerpackage   = $configoptions->{controller}{package};
    my $basepackage         = $configoptions->{basepackage};

    my $errorresponseclause = "";
    if (MAIN::checkOption($configoptions, 'useErrorResponse'))
    {
        $errorresponseclause = <<"EOT";

import ${basepackage}.exception.ErrorDetails;
import ${basepackage}.exception.ErrorResponse;
EOT
    }

    my $completeclass = <<"EOT";
package ${controllerpackage}.controlleradvice;

import org.json.JSONObject;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.ControllerAdvice;

import ${basepackage}.exception.ControllerAbstractExceptionAdvice;
${errorresponseclause}
import ${controllerpackage}.${classname};

\@ControllerAdvice(assignableTypes = ${classname}.class)
\@Component
public class ${classname}ExceptionAdvice extends ControllerAbstractExceptionAdvice
{
	\@Override
	public String getMessage()
	{
		return ERROR_MESSAGE;
	}
}
EOT

    $classname .= "ExceptionAdvice";

    ##
    #   if we are not writing output classes to the file system,
    #   dump the class text to stdout and return
    ##
    if ($outputpath eq '-')
    {
        print $completeclass;
        return;
    }

    printf "output : %s\n", "${classname}.java" if MAIN::checkOption($configoptions, 'verbose', 'flags');
    FileInterface::writeFile("${outputpath}/controlleradvice", "${classname}.java", $completeclass);

    ##
    #   generate an alternative Advice class, compounding references to all controller classes
    ##

    $classname = "CommonMethodControllerExceptionAdvice";

    my $importedcontrollerclasses   = "";
    my $controllerclassnames        = "";

    opendir(my $dh, $outputpath);
    my @filenamelist = sort grep /\.java$/, readdir($dh);
    closedir($dh);

    my $delim = " ";
    for my $filename (@filenamelist)
    {
        my $temp                    = $filename;
        $temp                       =~ s/\.java//i;
        $controllerclassnames      .= "       ${delim}${temp}.class\n";
        $importedcontrollerclasses .= "import ${controllerpackage}.${temp};\n";
        $delim                      = ",";
    }

    $completeclass = <<"EOT";
package ${controllerpackage}.controlleradvice;

import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.ControllerAdvice;

import ${basepackage}.exception.ControllerAbstractExceptionAdvice;
${importedcontrollerclasses}
\@ControllerAdvice(assignableTypes = {
${controllerclassnames}    })
\@Component
public class CommonMethodControllerExceptionAdvice extends ControllerAbstractExceptionAdvice
{
	\@Override
	public String getMessage()
	{
		return ERROR_MESSAGE;
	}
}
EOT

    printf "output : %s\n", "${classname}.java" if MAIN::checkOption($configoptions, 'verbose', 'flags');
    FileInterface::writeFile("${outputpath}/controlleradvice", "${classname}.java", $completeclass, { overwrite => 'true' });
}

##
#   generateControllerClass :
#
#   generate a controller class exposing all endpoints and methods from the swagger file(s)
##
sub generateControllerClass
{
    my ($tag) = @_;

    my $classname;
    my $endpointsInScope  = [];
    my $filterByTag       = 0;
    if (!defined $tag)
    {
        $classname          = deriveControllerClassname();
        @$endpointsInScope  = keys %$collectedEndpoints;
    }
    else
    {
        die "INTERNAL ERROR : $tag not found in \$taggedEndpoints" if !exists $taggedEndpoints->{$tag};
        my $subset   = $taggedEndpoints->{$tag};
        map {
            push @$endpointsInScope, $_;
        } sort keys %$subset;
        $filterByTag = 1;
        $classname   = ucfirst Parser::addCamelSuffix($tag, "Controller");
    }

    my $outputpath       = $configoptions->{controller}{path};

    my $basepath         =  $inputhash->{basePath}                     if exists $inputhash->{basePath};
    my $title            =  $inputhash->{info}{title}                  if exists $inputhash->{info}{title};
    $title               =~ s/[\r\n]//g                                if defined $title;

    my $dtoclassimports  =  {};
    my $classmethods     =  "";
    my $svcinfo          =  [];

    my $classdescription = $inputhash->{info}{description}             if exists $inputhash->{info} && exists $inputhash->{info}{description};
    $classdescription    = formatDescription('', $classdescription, 1) if defined $classdescription;

    for my $endpoint (@$endpointsInScope)
    {
        my @methodlist = keys %{$collectedEndpoints->{$endpoint}};

        for my $method (sort grep !/parameters/, @methodlist)
        {
            next if $filterByTag && !exists $taggedEndpoints->{$tag}{$endpoint}{$method};
            my $ctrlinfo   = generateControllerClassMethod($endpoint, $method, $dtoclassimports, $classname, $tag);
            $classmethods .= $ctrlinfo->{implementation};
            push @$svcinfo, $ctrlinfo->{invocation} if exists $ctrlinfo->{invocation};
        }
    }

    ##
    #   add the information to an intermediate array
    ##
    push @{$collectedClassInformation->{controller}}, { serviceinfo => $svcinfo, methods => $classmethods, imports => $dtoclassimports, basepath => $basepath, title => $title };

    ##
    #   start generating output file(s) from the collected information
    #   if this is the last swagger input file.
    ##
    if ($configoptions->{outputstate} != 0)
    {
        $dtoclassimports = {};
        $classmethods    = "";

        my $basepathhash = {};
        my $titlehash    = {};

        for my $c (@{$collectedClassInformation->{controller}})
        {
            map {$dtoclassimports->{$_} = 1;} keys %{$c->{imports}} if exists $c->{imports};

            $classmethods .= $c->{methods};

            $basepathhash->{$c->{basepath}} = 1 if exists $c->{basepath} && defined $c->{basepath};
            $titlehash->{$c->{title}}       = 1 if exists $c->{title}    && defined $c->{title};
        }

        if (!MAIN::checkOption($configoptions, 'clusterByTag'))
        {
            die "controller class name missing" if !exists $configoptions->{classnames}{controller};
            $classname = $configoptions->{classnames}{controller};
        }

        $basepath = join '::', keys %$basepathhash;
        $title    = join '::', keys %$titlehash;

        my $classheader   = generateControllerClassHeader($configoptions, $classname, $tag, $dtoclassimports, $basepath, $title, $classdescription);
        my $completeclass = $classheader->{header} . $classmethods . "\n}\n";

        if ($outputpath eq '-')
        {
            print $completeclass;
        }
        else
        {
            printf "output : %s\n", "${classname}.java" if MAIN::checkOption($configoptions, 'verbose', 'flags');
            FileInterface::writeFile($outputpath, "${classname}.java", $completeclass);
        }

        if (MAIN::checkOption($configoptions, 'generateControllerAdvice'))
        {
            generateControllerAdviceClass($classname, $outputpath);
        }
    }

    my $result = { classname => $classname, serviceinfo => $svcinfo, imports => $dtoclassimports };
    $result->{tag} = $tag if defined $tag;

    return $result;
}

sub generaterControllerAndServiceClass
{
    my ($tag) = @_;

    my $svcinfo = {};

    ##
    #   generate controller classes (exposing REST endpoints)
    ##
    if (MAIN::checkOption($configoptions, 'generateController'))
    {
        $svcinfo = generateControllerClass($tag);
    }

    ##
    #   generate service class stub (implementing the business logic for handling REST requests)
    ##
    if (MAIN::checkOption($configoptions, 'generateService'))
    {
        generateServiceClass($svcinfo);
    }
}

########################################################################
##
#       Process a single swagger file
##
########################################################################

package SwaggerHandler;

##
#   addRelativePackage :
#
#   add a relative package to base package and path
##
sub addRelativePackage
{
    my ($key, $package, $path, $default) = @_;

    # strip trailing delimiter characters
    $path    =~ s/\/$//;
    $package =~ s/\.$//;

    if (exists $configoptions->{relativepackage} && exists $configoptions->{relativepackage}{$key})
    {
        my $relpack =  $configoptions->{relativepackage}{$key};
        $relpack    =~ s/^\.//;

        my $relpath =  $relpack;
        $relpath    =~ s/\./\//g;
        $package   .=  ".${relpack}";
        $path      .=  "/${relpath}" if $path ne '-';
    }
    else
    {
        if (defined $default && $default ne "")
        {
            $package .= ".${default}";
            $path    .= "/${default}" if $path ne '-';
        }
    }

    return { package => $package, path => $path };
}

##
#   dumpTaggedEndpoints
#
#   dump a listing of endpoints per controller class
##
sub dumpTaggedEndpoints
{
    return if scalar keys %$taggedEndpoints == 0;

    printf "%s\n", '=' x 132;
    printf "Endpoints per Controller\n\n";

    my $widthcol1 = -1;
    map {
        $widthcol1 = length($_) if length($_) > $widthcol1;
    } keys %$taggedEndpoints;

    map {
        my $endpoint = $_;
        my $pad = ' ' x ($widthcol1 - length($endpoint));
        my $temp = $endpoint;
        map {
            printf "%s%s : %s\n", $temp, $pad, $_;
            $temp =~ s/./ /g;
        } sort keys %{$taggedEndpoints->{$endpoint}};
    } sort keys %$taggedEndpoints;
}

##
#   processInputFile :
#
#   - collect the composite data types from the swaggerfile
#     in a symbol table
#   - generate model java classes with sample instantiation statements
#   - controller and service classes
##
sub processInputFile
{
    die "'paths' missing from swagger file" if !exists $inputhash->{paths};

    my $endpoints = $inputhash->{paths};

    if ( MAIN::checkOption($configoptions, 'plaindump', 'flags') )
    {
        SwaggerUtils::dumpItem(0, 'SWAGGER', $inputhash);
        return;
    }

    if ( MAIN::checkOption($configoptions, 'dumpswaggerdefinitions', 'flags') )
    {
        SwaggerUtils::dumpSwaggerfile($endpoints);
        return;
    }

    ##
    #   1st pass : collect endpoint information in the symbol table
    ##
    if (exists $configoptions->{singleendpoint})
    {
        my $endpoint =  $configoptions->{singleendpoint};
        $endpoint    =~ s/^\///;
        $endpoint    =  "/${endpoint}";
        Parser::processEndPoint($endpoint);
    }
    else
    {
        for my $endpoint (sort keys %$endpoints)
        {
            Parser::processEndPoint($endpoint);
        }
    }

    SymbolTable::dumpSymboltable() if MAIN::checkOption($configoptions, 'dumpsymboltable', 'flags') && $configoptions->{outputstate};

    ##
    #   return if we just dump the parsed information to stdout
    ##
    return if MAIN::checkOption($configoptions, 'reportonly', 'flags');

    ##
    #   prepare package names for the generated classes
    ##
    my $basepackage        = $configoptions->{basepackage};

    my $sharedpackage      = $basepackage;
    my $distinctpackage    = $basepackage;

    ##
    #   convert the basepackage to a relative path
    #   and attach that to the base directory.
    ##
    my $outputpath         = $configoptions->{basedirectory};
    my $temp               = $basepackage;
    $temp                  =~ s/\./\//g;
    $outputpath           .= "/${temp}" if $outputpath ne '-';

    my $sharedoutputpath   = $outputpath;
    my $distinctoutputpath = $outputpath;

    if (MAIN::checkOption($configoptions, 'dedicatedPackage'))
    {
        die "neither 'basePath' nor 'paths' defined in swagger file" if !exists $inputhash->{paths} && !exists $inputhash->{basePath};

        my $path;
        if (exists $inputhash->{paths} && scalar keys %{$inputhash->{paths}} != 1)
        {
            die "'basePath' is missing with multiple 'paths'" if !exists $inputhash->{basePath};
            $path = $inputhash->{basePath};
        }
        else
        {
            $path = (keys %{$inputhash->{paths}})[0];
        }

        my $typePrefix   =  lc Parser::generateTypePrefix($path);

        $basepackage     =~ s/(.*)\.+$/$1/;
        $sharedpackage   =  $basepackage;
        $distinctpackage =  "${basepackage}.${typePrefix}";

        if ($outputpath ne '-')
        {
            $outputpath         =~ s/(.*)\/+$/$1/;
            $sharedoutputpath   =  $outputpath;
            $distinctoutputpath =  "${outputpath}/${typePrefix}";
            FileInterface::makeDirectory("$distinctoutputpath");
        }
    }

    my $modelpackage     = $distinctpackage;
    my $modeloutputpath  = $distinctoutputpath;

    if (MAIN::checkOption($configoptions, 'sharedModelPackage'))
    {
        $modelpackage    = "${sharedpackage}";
        $modeloutputpath = "${sharedoutputpath}";
    }

    my $modelinfo                = addRelativePackage('model',      $modelpackage,    $modeloutputpath);
    $modelpackage                = $modelinfo->{package};
    $modeloutputpath             = $modelinfo->{path};

    my $controllerinfo           = addRelativePackage('controller', $distinctpackage, $distinctoutputpath, 'controller');
    my $controllerpackage        = $controllerinfo->{package};
    my $controlleroutputpath     = $controllerinfo->{path};

    my $serviceinfo              = addRelativePackage('service',    $distinctpackage, $distinctoutputpath, 'service');
    my $servicepackage           = $serviceinfo->{package};
    my $serviceoutputpath        = $serviceinfo->{path};

    $configoptions->{model}      = { package => $modelpackage,      path => $modeloutputpath };
    $configoptions->{service}    = { package => $servicepackage,    path => $serviceoutputpath };
    $configoptions->{controller} = { package => $controllerpackage, path => $controlleroutputpath };

    if ($outputpath ne '-')
    {
        FileInterface::makeDirectory("$modeloutputpath");

        if (MAIN::checkOption($configoptions, 'generateController'))
        {
            FileInterface::makeDirectory("$controlleroutputpath");
            FileInterface::makeDirectory("${controlleroutputpath}/controlleradvice") if MAIN::checkOption($configoptions, 'generateControllerAdvice');
        }

        FileInterface::makeDirectory("$serviceoutputpath") if MAIN::checkOption($configoptions, 'generateService');
    }

    ##
    #   2nd pass : generate classes from the collected information
    ##

    ##
    #   generate model classes (abstract data types for use in request and response bodies)
    #   from the symboltable entries.
    ##
    for my $classname (sort keys %$symboltable)
    {
        CodeGenerator::generateModelClassDefinition($classname);
    }

    ##
    #   generate controller and services classes either from
    #   the complete '$collectedEndPoints' table or filtered
    #   by a tag from the '$taggedEndpoints' table.
    ##
    if (MAIN::checkOption($configoptions, 'clusterByTag'))
    {
        for my $tag (sort keys %$taggedEndpoints)
        {
            CodeGenerator::generaterControllerAndServiceClass($tag);

            ##
            #   reset the information on the previous class
            #   to avoid crosstalk between controller classes
            ##
            $collectedClassInformation->{controller} = [];
        }

        dumpTaggedEndpoints() if MAIN::checkOption($configoptions, 'verbose', 'flags');
    }
    else
    {
        CodeGenerator::generaterControllerAndServiceClass();
    }
}

########################################################################
##
#       Help screen
##
########################################################################

package MAIN;

sub printHelpScreenAndExit
{
    my $usage = "usage: $scriptName [-dhlmsuvx] [-c <classnames>] [-o <configoptions>] -b <output path> -p <basepackage> [-e <endpoint>] [-i <json|yaml>] -|<infile1>[ <infile2> ...]";

    my $helptext = <<"EOT";
    ${usage}

    generate model, controller and service classes and model class instantiation code for all requests from an api's swagger file.

    Options:

    -d  dump the contents of the swaggerfile and exit
    -h  print this help screen
    -l  list endpoints and request bodies only. don't generate any code.
    -m  don't abort when samenamed types from different input files mismatch.
    -s  dump the symboltable after the inputfile is parsed.
    -u  read/write files utf-8 encoded
    -v  increase verbosity
    -x  append path-derived suffixes to output path and basepackage
    -z  plain dump of the swagger file, without any parsing

    Arguments:

    -b  base directory for generated output files. '-' for stdout.
    -c  relative package and class names for model, controller and service classes :
        Example :
              [model=controller.model]|controller=[rest.controller.]EchoController|service=[rest.client.]EchoService
        CAVEAT: to define a relative model package add a '.', e.g. model=.dto
    -e  generate interface classes for a single endpoint
    -i  specify the input format when piping a swagger file to stdin
    -p  base package to generate the classes in (defaults to: default)
        the base package will be converted to a relative path and added to the base directory
    -o  configuration options delimited by '|' characters. supported options are :

        ignoreMismatches         : don't abort when samenamed types from different input files mismatch. equivalent to -m
        skipAuthorization        : suppress 'Authorization' parameters carrying OAuth2 tokens
        extendBaseType           : for polymorph declarations: extend the base type instead of merging all attributes in a single class
        attribCardinality        : add a mandatory/optional comment after a class attribute
        jsonCardinality          : add a (required=[true|false]) annotation to a \@JsonProperty annotation
        jsonIncludeNonNull       : suppress serialization of empty/null class attributes
        xmlSerialization         : add annotations for (de)serialization (from)/to xml in the model classes
        xmlContent               : add 'produces' annotations for application/xml in the controller classes
        xmlAccept                : add 'consumes' annotations for application/xml in the controller classes
        clusterByTag             : cluster the endpoints into controller classes by the 'tag' attribute
                                   this overrides any class names specified in the -c option
        generateController       : generate a controller class exposing the REST endpoints
        generateControllerAdvice : generate controller advice classes
        generateService          : generate a service class that implements the operations
        generateClient           : generate instantiation statements for all exposed model classes
        sharedModelPackage       : generate model classes in a single package. write new model class version on collision.
        dedicatedPackage         : append path-derived suffixes to output path and basepackage. equivalent to -x
                                   this will prevent collisions between different implementations of like-named client classes
        useErrorResponse         : use a generic class 'ErrorResponse' instead of generated specific classes for all responses not 1xx or 2xx
        useArrayResponseType     : use a synthetic array class in a response instead of the (default) 'responseContainer = List' annotation

                                   openapi 3.0 introduced new response layouts 'links' and 'content'.
                                   code generation is designed for either one with 'content' the default.
        enforceLinksResolution   : enforce resolution of the 'link' component of a response

    The generator resolves '\$ref' references to external or remote files in the format <file>[#/<reference>] where <file> is either a
    filename relative to the directory of the primary input file or a url for a remote file. '#/<reference' is a regular local reference.
    The definitions from the external file should come in one of the predefined sections (definitions, components, links, parameters).
    If the section is omitted, it defaults to the input file's containing folder (or to 'definitions' if the folder name is not one of
    the predefined sections).
    If the external file holds only a single definition, the <reference> in the '\$ref' clause is optional and can be omitted.
    It defaults to the definition from the file. If the external file holds multiple definitions and the reference is omitted from the
    '\$ref' clause, the processing run will fail with an error message.

    Example for generating classes from a downloaded swaggerfile and writing them to stdout :

    curl -s -X GET http://openshift-echoservice-dk0429-a.router.default.svc.cluster.local:80/v2/api-docs \|\\
    swaggercodegen.pl -sb -b ./src/main/java -o 'jsonCardinality|jsonIncludeNonNull|useErrorResponse|generateController|generateControllerAdvice\\
                                   |generateService|generateClient|extendBaseType|clusterByTag'
                       -c 'model=controller.model|controller=rest.controller.EchoController|service=rest.client.EchoService'
                       -p com.db.payment.service.directive.adapter.scgc -i json -
EOT

    print "${helptext}\n";

    exit 0;
}

# ------------------------------------------------------------------------------------ #
# process generation options                                                           #
# ------------------------------------------------------------------------------------ #

sub compileGenerationOptions
{
    my ($options) = @_;

    my $co = {};

    $co->{basedirectory}                       = $options->{b} if exists $options->{b};
    $co->{singleendpoint}                      = $options->{e} if exists $options->{e};
    $co->{basepackage}                         = $options->{p} if exists $options->{p};

    ##
    #   map switches to long names
    ##
    $co->{flags} = {};
    $co->{flags}{dumpswaggerdefinitions}       = 1 if exists $options->{d};
    $co->{flags}{reportonly}                   = 1 if exists $options->{l};
    $co->{flags}{dumpsymboltable}              = 1 if exists $options->{s};
    $co->{flags}{verbose}                      = 1 if exists $options->{v};
    $co->{flags}{plaindump}                    = 1 if exists $options->{z};

    $co->{generationoptions}                   = {};

    if (exists $options->{o})
    {
        my $var =  $options->{o};
        $var    =~ s/['"]//g;

        map {
            my $option = lc $_;
            $option =~ s/^\s+|\s+$//g;
            $co->{generationoptions}{$option} = 1;
        } split '[\|;]', $var;
    }

    ##
    #   downward compatibility of options and switches
    ##
    $co->{generationoptions}{ignoreMismatches} = 1 if exists $options->{m};
    $co->{generationoptions}{dedicatedPackage} = 1 if exists $options->{x};

    $co->{classnames} = {};
    if (exists $options->{c})
    {
        my $var = $options->{c};
        $var =~ s/['"]//g;

        map {
            my @v = split '=', $_;
            my $relativepackage = $v[1];
            my $classname;
            if ($relativepackage =~ m/\./)
            {
                my @w = split '\.', $relativepackage;
                $classname = pop(@w) if $v[0] !~ /^model$/i && $relativepackage !~ /.+\.$/;
                $co->{relativepackage}{lc $v[0]} = join '.', @w;
            }
            else
            {
                $classname = $relativepackage;
            }

            if (defined $classname)
            {
                warn "clusterByTags overrides controller/service classnames from -c option" if exists $co->{generationoptions}{lc 'clusterByTag'};
                $co->{classnames}{lc $v[0]} = $classname;
            }
        } split '[\|;]', $var;
    }

    return $co;
}

sub checkOption
{
    my ($configoptions, $toTest, $section) = @_;

    if (defined $configoptions && ref($configoptions) eq 'HASH' && defined $section && ref $section eq '' && exists $configoptions->{$section})
    {
        return exists $configoptions->{$section}{lc $toTest};
    }

    if (defined $configoptions && ref($configoptions) eq 'HASH' && exists $configoptions->{generationoptions})
    {
        return exists $configoptions->{generationoptions}{lc $toTest};
    }

    return 0;
}

# ------------------------------------------------------------------------------------ #
# main                                                                                 #
# ------------------------------------------------------------------------------------ #

# parse options

my $optStr  = 'b:c:de:hi:lo:p:suvxz';
my $options = {};
if (!getopts("$optStr", $options))
{
    print "Invalid option(s) given\n";
    exit -4;
}

##
#   ensure we have an internal representation of 'Boolean' that is independent of input file format
##
$YAML::XS::Boolean = "JSON::PP";

printHelpScreenAndExit               if exists $options->{h} || scalar keys %$options == 0;

$options->{p}  = 'default'           if !exists $options->{p};
$configoptions = compileGenerationOptions($options);

die "output path undefined"          if !exists $configoptions->{basedirectory};
my $outputpath = $options->{b};
die "$outputpath does not exist"     if $outputpath ne '-' && !-d $outputpath;

if (checkOption($configoptions, 'useErrorResponse'))
{
    $classesSuppressedFromImport->{uc "ErrorResponse"} = 'true';
    $classesSuppressedFromImport->{uc "ErrorDetails"}  = 'true';
}

##
#	process all files listed on the command line
##
while (my $infile = shift @ARGV)
{
    printf "input  : %s\n", $infile                           if checkOption($configoptions, 'verbose', 'flags');;

    $configoptions->{inputfile}   = $infile;
    $configoptions->{outputstate} = !scalar @ARGV;

    my $inputformat     = $options->{i}                       if exists $options->{i};
    $inputformat        = FileInterface::findFormat($infile)  if !defined $inputformat;
    die "inputformat undefined"                               if !defined $inputformat;
    die "unsupported inputformat $inputformat"                if !exists $readInput->{$inputformat};

    $inputhash          = $readInput->{$inputformat}->($infile, $options);

    die "unsupported swagger version $inputhash->{swagger}"   if exists $inputhash->{swagger} && $inputhash->{swagger} > swagger_version;

    $collectedEndpoints = {};

    SwaggerHandler::processInputFile();
}

exit 0
