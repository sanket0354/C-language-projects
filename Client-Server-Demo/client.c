/********************************************************************************
   PROGRAM:  client.c 
   AUTHOR:   Sanket Patel and Anita Rezakhani
   DATE:     12 DECEMBER 2015 
   TOPIC:    Socket Programming 
   NOTES:    - client takes server's hostname and port as command line argument 
	       to connect to server.
	     - Once it connects, it can pass a URL to server to open in a browser
	     - client disconnects by pressing 'q'
	     - Unless 'q' is pressed, client continuosly asks for another URL
	       to be openned at server's side. (it has a functionality of 	
	       openning multiple websites)
*****************************************************************************/

#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h> 
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>

/*********************************************************************
 * Function Prototypes
 ********************************************************************/
void display_usage( char *, char * );
void error(char *);

/*********************************************************************
 * Defines
 ********************************************************************/
#define OPTIONS "HOSTNAME PORT"
#define BUF_SIZE 1024
#define h_addr h_addr_list[0] 

/*********************************************************************
 * Main
 ********************************************************************/
int main(int argc, char *argv[]) {

	/*
	 * Same as server
	 */
	int sockfd, portno, n;
    	struct sockaddr_in serv_addr;
    	char buffer[BUF_SIZE];
	int length;
	

	/*
	 * Defines a host on the internet
	 * struct  hostent {
         *     char    *h_name;        // official name of host
         *     char    **h_aliases;    // alias list
         *     int     h_addrtype;     // host address type AF_INET
         *     int     h_length;       // length of address in bytes
         *     char    **h_addr_list;  // list of addresses from name server 
         *    #define h_addr  h_addr_list[0]  // address, for backward compatiblity
         *};	
	 */
	struct hostent *server;


    	if (argc < 3) {
		display_usage( argv[0], OPTIONS );
		exit( EXIT_FAILURE );
    	}
	
    	portno = atoi(argv[2]);

	/*
	 * Step 1:  Create a socket 
	 */
    	if ( ( sockfd = socket(AF_INET, SOCK_STREAM, 0) ) < 0 )
	        error("ERROR opening socket");

	/*
	 * Remember DNS?  
	 * Given the hostname, returns the IP address
	 */
	if ( (server = gethostbyname(argv[1]) ) == NULL ) {
        	fprintf(stderr,"ERROR, no such host\n");
        	exit(0);
    	}

	/*
	 * Init server address
	 */
	bzero((char *) &serv_addr, sizeof(serv_addr));

	serv_addr.sin_family = AF_INET;
	bcopy((char *)server->h_addr, (char *)&serv_addr.sin_addr.s_addr,server->h_length);
	serv_addr.sin_port = htons(portno);

	/*
	 * Step 2:  Connect
	 */


    	if (connect(sockfd,(struct sockaddr *)&serv_addr,sizeof(serv_addr)) < 0) 
        	error("ERROR connecting");
	
	
	printf("Press q whenever you want to quit \n");

	while(length != 1 && buffer[0] != 'q'){
		
		printf("Enter the URL : \n");

		bzero(buffer,BUF_SIZE);	
		fgets(buffer,BUF_SIZE,stdin);

		if((n = write(sockfd,buffer,strlen(buffer))) < 0)
			error("Error reading from socket");
	
		length = strlen(buffer);
	}
	
	close( sockfd );
		
	return 0;
}

/*********************************************************************
 * display_usuage
 ********************************************************************/
void display_usage( char * prog, char *opts ) {

	fprintf(stderr, "usage: %s %s\n", prog, opts );
	return;

}

/*********************************************************************
 * error & exit
 ********************************************************************/
void error(char *msg) {

    perror(msg);
    exit( EXIT_FAILURE );
}
