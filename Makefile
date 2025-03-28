CC := gcc

CFLAGS := -std=c99 -Wall -Wextra

RELEASE := 0
LDFLAGS := -fsanitize=address 
APP_NAME := receval

SRCS := receval.c parser.c tokenizer.c utils.c
HEADERS := common.h ident.h arena.h da.h tokenizer.h parser.h
OBJDIR := obj
OBJS = $(patsubst %.c, $(OBJDIR)/%.o, $(SRCS))

ifeq ($(RELEASE), 1)
	CFLAGS += -g -O3 -D NDEBUG
else
	CFLAGS += -g -O0 -fsanitize=address
endif

all: $(OBJS)
	$(CC) $(LDFLAGS) $^ -o $(APP_NAME)

run: all
	./$(APP_NAME)

$(OBJDIR)/%.o: %.c $(HEADERS)
	mkdir -p $(OBJDIR)
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm $(APP_NAME) $(OBJDIR)/*.o


.PHONY: clean
