Setting up a multipass VM to run DPDK

**1. create the vm**
```
multipass launch 22.04 --name dpdk --cpus 4 --disk 10G --memory 8G
multipass mount `pwd` dpdk:/mnt/host
```
**2. log into it**
```
multipass shell dpdk
```
**3. install dpdk**
```
cd /mnt/host
sudo ./scripts/dpdk_init.sh
```
**4. set up hugepages**
(run from inside of vm, on each reboot)
```
cd /mnt/host
sudo ./scripts/env_init.sh
```
