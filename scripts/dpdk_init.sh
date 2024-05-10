echo "==== installing dependencies ===="
sudo apt update
sudo apt install make clang llvm net-tools scapy tcpreplay libelf-dev libpcap-dev build-essential linux-tools-common linux-tools-generic tcpdump m4 libc6-dev-i386 meson ninja-build python3-pyelftools libnuma-dev 
echo "==== downloading dpdk ===="
wget https://fast.dpdk.org/rel/dpdk-24.03.tar.xz
tar -xJf dpdk-24.03.tar.xz
cd dpdk-24.03
echo "==== building dpdk ===="
meson setup build
cd build
ninja
echo "==== installing dpdk ===="
sudo meson install
sudo ldconfig