Swagger Code Generator for OpenAPI 3.0 implemented in Perl

usage: swaggercodegenv2.pl [-dhlmsuvx] [-c <classnames>] [-o <configoptions>] -b <output path> -p <basepackage> [-e <endpoint>] [-i <json|yaml>] -|<infile1>[ <infile2> ...]

generate model, controller and service classes and sample client instantiation code for all requests from an api's swagger file.

Options:

-d  dump the contents of the swaggerfile and exit
-h  print this help screen
-l  list endpoints and request bodies only. don't generate any code.
-m  don't abort when samenamed types from different input files mismatch.
-r  dump the definitions from the swaggerfile interleaved with the derived classes
-s  dump the symboltable after the inputfile is parsed.
-u  read/write files utf-8 encoded
-v  increase verbosity
-x  append path-derived suffixes to output path and basepackage
-z  plain dump of the swagger file, without any parsing

Arguments:

-b  base directory for generated output files. '-' for stdout.
-c  relative package and class names for model, controller and service classes :
          [model=controller.dto]|controller=[rest.controller.]EchoController|service=[rest.client.]EchoService
    CAVEAT: to define a relative model package add a '.', e.g. model=.dto
-e  generate interface classes for a single endpoint
-i  specify the input format when piping a swagger file to stdin
-p  base package to generate the classes in (defaults to: default)
    the base package will be converted to a relative path and added to the base directory
-o  configuration options delimited by '|' characters. supported options are :

    ignoreMismatches         : don't abort when samenamed types from different input files mismatch. equivalent to -m
    skipAuthorization        : suppress 'Authorization' parameters carrying OAuth2 tokens
    extendBaseType           : extend the base type instead of adding new attributes for polymorph declarations
    attribCardinality        : add a mandatory/optional comment after a class attribute
    jsonCardinality          : add a (required=[true|false]) annotation to a @JsonProperty annotation
    jsonIncludeNonNull       : suppress serialization of empty/null class attributes
    clusterByTag             : cluster the endpoints into controller classes by the 'tag' attribute
                               this overrides any class names specified in the -c option
    generateController       : generate a controller class exposing the REST endpoints
    generateControllerAdvice : generate controller advice classes
    generateService          : generate a service class that implements the operations
    generateClient           : generate code for creating request bodies for POST requests
    sharedModelPackage       : generate model classes in a single package. write new model class version on collision.
    dedicatedPackage         : append path-derived suffixes to output path and basepackage. equivalent to -x
                               this will prevent collisions between different implementations of like-named client classes
    useErrorResponse         : use a generic class 'ErrorResponse' instead of generated specific classes for all responses not 1xx or 2xx
    useArrayResponseType     : use a synthetic array class in a response instead of the (default) 'responseContainer = List' annotation

                               openapi 3.0 introduced new response layouts 'links' and 'content'.
                               code generation is designed for either one with 'content' the default.
    enforceLinksResolution   : enforce resolution of the 'link' component of a response

The generator resolves '$ref' references to external files in the format <file>#/<reference> where <file> is  
a filename relative to the directory of the primary input file and '#/<reference' is a regular local reference.
The definitions from the external file should come in one of the predefined sections (definitions, components, 
links, parameters). If the section is omitted, it defaults to 'definitions'.                                   
If the external file holds only a single definition, the <reference> in the '$ref' clause can be omitted.     
It defaults to the definition from the file. If the external file holds multiple definitions and the reference 
is omitted from the '$ref' clause, the processing run will fail with an error message.                        

Example for generating classes from a downloaded swaggerfile and writing them to stdout :

curl -s -X GET http://openshift-echoservice-dk0429-a.router.default.svc.cluster.local:80/v2/api-docs |\
swaggercodegen.pl -sb -b ./src/main/java -o 'jsonCardinality|jsonIncludeNonNull|useErrorResponse|generateController|generateControllerAdvice\
                               |generateService|generateClient|extendBaseType|clusterByTag' 
                   -c 'model=controller.model|controller=rest.controller.EchoController|service=rest.client.EchoService'
                   -p com.db.payment.service.directive.adapter.scgc -i json -
