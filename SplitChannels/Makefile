# SplitChannels makefile

CC=gcc
CCFLAGS=
LDFLAGS=
LIBS=

EXECNAME=SplitChannels

SRCS=$(shell ls src/*.c)
OBJS=${SRCS:.c=.o}

all: $(EXECNAME)

$(EXECNAME): $(OBJS)
	@echo "Linking..."
	@$(CXX) $(LDFLAGS) $(OBJS) -o $@ $(LIBS)

.c.o:
	@echo "Compiling $<..."
	@$(CC) -c $(CCFLAGS) $< -o $@

clean:
	@rm -f src/*.o
	@rm -f $(EXECNAME)
