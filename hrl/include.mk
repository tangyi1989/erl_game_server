####################################################################
## @author <jjchen.lian@gmail.com>
## @datetime 2013-09-23
## @description 公用makefile文件
##
####################################################################

SHELL := /bin/bash
.PHONY: all dialyzer clean hrl

HERE := $(shell pwd | cut -d "/" -f1-3)
#$(warning "the value of LOCAL_PATH is$(HERE)")  

SHELL_DIR := $(shell cd $(BASE) && pwd)

## ebin 根目录,暂时写死了
APP_EBIN_ROOT := $(HERE)/ebin/

ERL := erl
ERLC := $(ERL)c
ERLC2 := $(ERL)c +debug_info
EMULATOR := beam

INCLUDE_DIRS := include

##支持-pa共享库
EBIN_DIR := $(APP_EBIN_ROOT)/common
##ELIB_DIR := $(APP_EBIN_ROOT)/library

##指定编译时查找hrl中的文件
ERLC_FLAGS := -I $(INCLUDE_DIRS) -I ../../../hrl -pa $(EBIN_DIR)
##这里可以通过 make DEBUG=true来达到打开debug_info选项的目的
ifdef DEBUG
  ERLC_FLAGS += +debug_info
endif

ifdef TEST
  ERLC_FLAGS += -DTEST
endif


##所有的erl源码文件
ERL_SOURCES := $(wildcard $(SRC_DIRS))
ERL_SOURCES2 := $(addprefix $(EBIN_DIR)/,$(notdir $(ERL_SOURCES)))
##所有对应的erl beam文件
ERL_OBJECTS := $(ERL_SOURCES2:%.erl=%.$(EMULATOR))
##输出文件
EBIN_FILES = $(ERL_OBJECTS)

all: hrl mk_dir $(EBIN_FILES)


clean:
	(rm -rf $(APP_EBIN_ROOT)/common/*)
	
common:
	@(cd ../common;$(MAKE))
	
mk_dir:
	@(mkdir -p $(APP_EBIN_ROOT)/common/)
	
debug: clean
	$(MAKE) DEBUG=true
