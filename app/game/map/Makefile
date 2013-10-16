####################################################################
## @author <tang_yi_1989@qq.com>
## @datetime 2013-09-23
## @description beam文件被放在ebin下或者ebin子目录下，所以启动时要
##				注意-pa参数是否设置正确
##
####################################################################
##指定当前项目在svn仓库中的名字
APP_NAME := map
SRC_DIRS := src/*.erl src/mod/*.erl unittest/*.erl
include ../../../hrl/include.mk

##使用通配符方式指定每个文件的编译规则
$(EBIN_DIR)/%.$(EMULATOR): $(hrl) src/%.erl 
	$(ERLC2) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

$(EBIN_DIR)/%.$(EMULATOR): $(hrl) src/mod/*.erl
	$(ERLC2) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

$(EBIN_DIR)/%.$(EMULATOR): $(hrl) unittest/%.erl
	$(ERLC2) $(ERLC_FLAGS) -o $(EBIN_DIR) $<
