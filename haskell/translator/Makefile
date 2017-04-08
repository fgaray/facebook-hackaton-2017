build:
	stack build

run:
	stack exec translator

test:
	stack test

docstring:
	stack runghc doctest.hs

logcat:
	adb logcat

upload: .stack-work/install/x86_64-linux-nopie/lts-8.6/8.0.2/bin/translator-exec .stack-work/install/x86_64-linux-nopie/lts-8.6/8.0.2/bin/translator-exec.xz
	rm -f .stack-work/install/x86_64-linux-nopie/lts-8.6/8.0.2/bin/translator-exec.xz
	xz -9 .stack-work/install/x86_64-linux-nopie/lts-8.6/8.0.2/bin/translator-exec
	scp  .stack-work/install/x86_64-linux-nopie/lts-8.6/8.0.2/bin/translator-exec.xz azureuser@hackaton.cloudapp.net:~
	scp -C -r fb-chat azureuser@hackaton.cloudapp.net:~
	ssh azureuser@hackaton.cloudapp.net "rm -f translator-exec && unxz translator-exec.xz"

.stack-work/install/x86_64-linux-nopie/lts-8.6/8.0.2/bin/translator-exec:
	stack clean
	stack build
