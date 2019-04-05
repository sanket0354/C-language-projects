/********************************************************************************
   PROGRAM:  server.c 
   AUTHOR:   Sanket Patel and Anita Rezakhani
   DATE:     12 DECEMBER 2015 
   TOPIC:    Socket Programming 
   NOTES:    - Server creates a socket for multiple clients to connect to it at
	       a particular port number which clients should know where server is
	       running
	     - it listens continuously if any client wants to connect or not 
             - whenever a client connects, server reads the requested URL from it's buffer to open
	     - This server can have multiple clients (fork)
*****************************************************************************/

#define _XOPEN_SOURCE
#include <stdio.h>
#include <sys/types.h> 
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>
#include <sys/socket.h>
#include <signal.h>

/*********************************************************************
 * Function Prototypes
 ********************************************************************/
void display_usage( char *, char * );
void error(char *);
void open_url(char *url);
int catch_signal(int sig, void (*handler)(int));
void handle_interrupt(int sig);

/*********************************************************************
 * Defines
 ********************************************************************/
#define OPTIONS "PORT"
#define BUF_SIZE 1024
#define FOREVER 1

/*********************************************************************
 * Main
 ********************************************************************/
int main(int argc, char *argv[]) {

	/*
	 * sockfd, newsockfd:  file descriptors -- as when you use open
	 * portno:  port number to be used
	 * clilen:  size of the address of the client -- use in accept
	 * buffer:  Use to read from the socket
	 * serv_add, cli_add:  Internet address
	 */
	int sockfd, newsockfd, portno , n;
	char buffer[BUF_SIZE];
	socklen_t clilen;
	int length = 0;
	struct sockaddr_in serv_addr, cli_addr;
	pid_t pid;

	if (argc < 2) {
		display_usage( argv[0], OPTIONS );
		exit( EXIT_FAILURE );
	} 

	 if (catch_signal(SIGINT, handle_interrupt) == -1) {
	 	fprintf(stderr, "Can't map the handler");
		 exit(2);
	 }


	
	/*
	 * Step 1:  Create a socket 
         * socket( ) requieres 3 arguments:
 	 *       domain:  AF_UNIX --> Unix internal protocol -- same filesystem
	 *		  AF_INET --> Internel, any two host 
	 *       type_socket:
	 *                SOCK_STREAM --> sequence two way connection based on bytes
	 *                SOCK_DGRAM  --> datagrams, info read in chuncks
	 *       protocol: 
	 *                if 0 --> OS choose
	 *                
	 */
	if ( ( sockfd = socket(AF_INET, SOCK_STREAM, 0) ) < 0 ) 
		error( "Open Socket" );

	/*
	 * Init serv_add to zeros
	 */
	bzero( (struct sockaddr_in *) &serv_addr, sizeof(serv_addr) );

     	portno = atoi(argv[1]);

	/*
	 * Init serv_addr
	 *
	 * struct sockaddr_in {
         *        short   sin_family;
         *        u_short sin_port;
         *        struct  in_addr s_addr;
         *        char    sin_zero[8];
         * };
	 *
	 * s_add --> IP address of the host  INADDR_ANY is a constant with the IP 
	 */
     	serv_addr.sin_family = AF_INET;
     	serv_addr.sin_addr.s_addr = INADDR_ANY;
     	serv_addr.sin_port = htons(portno);
     
	/*
	 * Step 2:  Bind
	 * bind( ) binds a socket to an address
	 */

	if ( bind(sockfd, (struct sockaddr *) &serv_addr, sizeof(serv_addr)) < 0)  
		error("ERROR on binding");

	/*
	 * Step 3:  Listen to connections
	 * Allows the socket to listen on the socket for connections
	 * second argument is the number of connections that can be waiting while the process
	 * is handling a particular connection
	 * Cannot fail if sockfd is valid!
	 */

	while(FOREVER){
    	
		listen( sockfd, 5 );
	
	     	clilen = sizeof(cli_addr);

		/*
		 * Step 4:  Accept connections
		 * accept( ) blocks until a connection happens
		 * wakes up when a client connects
		 * Returns a new socket descriptor, all communication happens from/to this new fd
		 * cli_addr refers to the address of the client
		 */
		if ( (newsockfd = accept(sockfd, (struct sockaddr *) &cli_addr, &clilen)) < 0 ) 
			error("ERROR on accept");
	
		/*
		 * Init buffer with zeros 
		 */
	     	bzero(buffer,BUF_SIZE);

		/* SAME CODE UP TO HERE */
	
		switch(pid = fork()){

		case -1:
			error("fork");
			
		case 0:
			close ( sockfd ); /* closing parent socket in child */
			while(length != 1 && buffer[0] != 'q'){
				bzero(buffer,BUF_SIZE);	

				if((n = read(newsockfd,buffer,BUF_SIZE)) < 0 )
					error("Error reading from socket");
			
				length = strlen(buffer);
	
				if(length != 1 && buffer[0] != 'q')	
					open_url(buffer);

			}
	
			close(newsockfd);	
		break;
	

		}/*switch*/
	
	}/*end while forever*/
	
	
	close( sockfd );

	return 0; 

}

/*******************************************************************
* To open URL given by the client
*********************************************************************/

void open_url(char *url)
{
	char launch[255];
	sprintf(launch, "x-www-browser '%s' &", url);
	system(launch);

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

/*********************************************************************
 * 	int catch_signal(int sig, void (*handler)(int))
 ********************************************************************/
int catch_signal(int sig, void (*handler)(int))
	{
	 struct sigaction action;
	 action.sa_handler = handler;
	 sigemptyset(&action.sa_mask);
	 action.sa_flags = 0;
	 return sigaction (sig, &action, NULL);
	}

/**********************************************************************
 * handle_interrupt(int sig)
 **********************************************************************/
void handle_interrupt(int sig)
{
	puts ("( hit Ctrl+C ) Server is shutting down....\n");
/*	close(sockfd);*/
	exit(0);
}
