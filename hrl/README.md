这里的include.mk的项目路径暂时先这样写着吧
HERE := $(shell pwd | cut -d "/" -f1-3)
根据自己的项目路径先手动调一下 -f1-3这个选项，后面再来优化
