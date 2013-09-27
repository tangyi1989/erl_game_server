####################################################################
## @author jjchenlian <jjchen.lian@gmail.com>
## @datetime 2013-08-28
## @description beam文件被放在ebin下或者ebin子目录下，所以启动时要
##				注意-pa参数是否设置正确
##
####################################################################
##指定当前项目在svn仓库中的名字
APP_NAME := manager
SRC_DIRS := src/*.erl src/mod/*.erl
include ../../../hrl/include.mk

##使用通配符方式指定每个文件的编译规则
$(EBIN_DIR)/%.$(EMULATOR): $(hrl) src/%.erl 
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<
	
$(EBIN_DIR)/%.$(EMULATOR): $(hrl) src/mod/%.erl 
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<
