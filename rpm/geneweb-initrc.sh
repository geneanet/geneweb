#!/bin/sh
# chkconfig: 345 99 01
#
# gwd:       Starts the genealogy Server
#
# Version:      @(#) /etc/rc.d/init.d/gwd 1.0
#
# description: Starts and stops the genealogy Server at boot time and shutdown.
#
# processname: gwd
# hide: true

# Source function library.
if [ -f /etc/rc.d/init.d/functions ]; then
	. /etc/rc.d/init.d/functions
fi

# See how we were called.
case "$1" in
  start)
	echo "Starting GeneWeb Services:"
        touch /var/log/gwd.log /var/log/gwsetup.log
        chown geneweb /var/log/gwd.log /var/log/gwsetup.log
	cd /home/geneweb/gw/gw
	./gwd -log /var/log/gwd.log -daemon
	./gwsetup -daemon 2>> /var/log/gwsetup.log
	touch /var/lock/subsys/gwd
	;;
  stop)
	echo -n "Shutting down GeneWeb Services: "
	cd /home/geneweb/gw/gw
	killproc ./gwd
	killproc ./gwsetup
	rm -f /var/lock/subsys/gwd
	echo
	;;
  status)
	status gwd
	;;
  restart)
	echo -n "Restarting GeneWeb Services: "
	cd /home/geneweb/gw/gw
        killproc gwd
        killproc gwsetup
	echo
        touch /var/log/gwd.log /var/log/gwsetup.log
        chown geneweb /var/log/gwd.log /var/log/gwsetup.log
	./gwd -log /var/log/gwd.log -daemon
	./gwsetup -daemon 2>> /var/log/gwsetup.log
	touch /var/lock/subsys/gwd
	;;
  *)
	echo "*** Usage: gwd {start|stop|status|restart}"
	exit 1
esac

exit 0
