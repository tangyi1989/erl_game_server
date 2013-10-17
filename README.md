看了网上流传的一份《英雄远征》的代码，感觉很兴奋。
觉得服务端代码就应该这么写，而不应该充满万恶的回调。

然后就想写着玩玩看。
为了避免代码弄丢，就上传到了github上了，仅此而已。

编译程序：
./aids-helper make
./aids-helper clean

注意请在aids-helper和start_gateway.sh修改一下日志目录，这些都暂时先写死把
如果看到当前shell下面打印出网关节点启动成功那么说明管理节点和网关节点已经正常工作


使用lager开源项目,需要给每个项目erlang文件编译时加上编译参数
erlc -pa lager/ebin +'{parse_transform, lager_transform}' +'{lager_truncation_size, 1024}' file.erl
test_log.erl为测试文件
erl -sname xxx -pa path
