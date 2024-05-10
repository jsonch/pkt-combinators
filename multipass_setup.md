Setting up a multipass VM to run DPDK

**1. create the vm and get into it**
```
multipass launch 22.04 --name dpdk --cpus 4 --disk 10G --memory 8G
multipass mount `pwd` dpdk:/mnt/host
```
**2. install dpdk**
```
cd /mnt/host
sudo ./scripts/dpdk_init.sh
```
(run from inside of vm, or on whatever ubuntu machine you use)
**3. set up veth interfaces and hugepages**
```
cd /mnt/host
sudo ./scripts/env_init.sh
```
