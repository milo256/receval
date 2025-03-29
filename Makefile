CC := gcc
ASAN := 1
RELEASE := 0
APPNAME := receval

CFLAGS := -std=c99 -Wall -Wextra -Wno-missing-braces
LDFLAGS :=
DEBUGFLAGS := -g -O0
RELEASEFLAGS := -g -O0


OBJDIR := obj
SRCDIR := src

SRC_NAMES := main.c parser.c tokenizer.c eval.c common.c
HEADER_NAMES := parser.h tokenizer.h common.h expr.h

HEADERS := $(patsubst %, $(SRCDIR)/%, $(HEADER_NAMES))
OBJS := $(patsubst %.c, $(OBJDIR)/%.o, $(SRC_NAMES))


ifeq ($(RELEASE), 1)
	CFLAGS += $(RELEASEFLAGS)
else
	CFLAGS += $(DEBUGFLAGS)
ifeq ($(ASAN), 1)
	CFLAGS += -fsanitize=address
	LDFLAGS += -fsanitize=address
endif
endif

all: $(OBJS)
	$(CC) $(LDFLAGS) $^ -o $(APPNAME)

run: all
	./$(APPNAME)

$(OBJDIR)/%.o: $(SRCDIR)/%.c
	mkdir -p $(OBJDIR)
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm $(APPNAME) $(OBJDIR)/*.o

.PHONY: clean
