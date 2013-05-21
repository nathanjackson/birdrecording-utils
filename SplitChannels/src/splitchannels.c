#include <stdio.h>
#include <stdlib.h>

#include <unistd.h>

void usage(const char *exec)
{
	printf("Usage: %s -i <input-file> [-d output-directory]\n", exec);
	exit(1);
}

int main(int argc, char **argv)
{
	int opt;
	char *filename = NULL;
	char *outputdir = NULL;

	while((opt = getopt(argc, argv, "hi:d:")) != -1) {
		if(opt == 'i')
			filename = optarg;
		else if(opt == 'd')
			outputdir = optarg;
		else if(opt == 'h')
			usage(argv[0]);
		else if(opt == '?')
			usage(argv[0]);
	}

	if(filename == NULL) {
		printf("No input file specified!\n");
		usage(argv[0]);
	}

	if(outputdir == NULL) {
		outputdir = (char *) malloc(sizeof(char) * 512);
		getcwd(outputdir, 512);
	}
}
