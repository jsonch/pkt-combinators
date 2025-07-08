#ifndef _DPDK_IMPORTS_
#include <rte_eal.h>
#include <rte_ethdev.h>
#include <rte_cycles.h>
#include <rte_lcore.h>
#include <rte_mbuf.h>
#define _DPDK_IMPORTS_
#endif



typedef struct cfg_t {
	uint16_t rx_ring_size;
	uint16_t tx_ring_size;
	int      num_mbufs;
	int      mbuf_cache_size;
	uint16_t metadata_size;
	uint16_t num_tx_queues;
} cfg_t;


static inline int
port_init(cfg_t cfg, uint16_t port, struct rte_mempool *mbuf_pool)
{
	struct rte_eth_conf port_conf;
	// rx and tx rings per port. Each ring is a queue.
	const uint16_t rx_rings = 1;
    uint16_t tx_rings = cfg.num_tx_queues;

	uint16_t nb_rxd = cfg.rx_ring_size;
	uint16_t nb_txd = cfg.tx_ring_size;
	int retval;
	uint16_t q;
	struct rte_eth_dev_info dev_info;
	struct rte_eth_txconf txconf;

	if (!rte_eth_dev_is_valid_port(port))
		return -1;

	memset(&port_conf, 0, sizeof(struct rte_eth_conf));

	retval = rte_eth_dev_info_get(port, &dev_info);
	if (retval != 0) {
		printf("Error during getting device (port %u) info: %s\n",
				port, strerror(-retval));
		return retval;
	}

    // Check if requested TX queues are supported
    if (cfg.num_tx_queues > dev_info.max_tx_queues) {
        printf("Error: Requested %u TX queues, but device supports max %u\n",
                cfg.num_tx_queues, dev_info.max_tx_queues);
        return -1;
    }

	if (dev_info.tx_offload_capa & RTE_ETH_TX_OFFLOAD_MBUF_FAST_FREE)
		port_conf.txmode.offloads |=
			RTE_ETH_TX_OFFLOAD_MBUF_FAST_FREE;

	/* Configure the Ethernet device. */
	retval = rte_eth_dev_configure(port, rx_rings, tx_rings, &port_conf);
	if (retval != 0)
		return retval;

	retval = rte_eth_dev_adjust_nb_rx_tx_desc(port, &nb_rxd, &nb_txd);
	if (retval != 0)
		return retval;

	/* Allocate and set up 1 RX queue per Ethernet port. */
	for (q = 0; q < rx_rings; q++) {
		retval = rte_eth_rx_queue_setup(port, q, nb_rxd,
				rte_eth_dev_socket_id(port), NULL, mbuf_pool);
		if (retval < 0)
			return retval;
	}

	txconf = dev_info.default_txconf;
	txconf.offloads = port_conf.txmode.offloads;
	/* Allocate and set up 1 TX queue per Ethernet port. */
	for (q = 0; q < tx_rings; q++) {
		retval = rte_eth_tx_queue_setup(port, q, nb_txd,
				rte_eth_dev_socket_id(port), &txconf);
		if (retval < 0)
			return retval;
	}

	/* Starting Ethernet port. 8< */
	retval = rte_eth_dev_start(port);
	/* >8 End of starting of ethernet port. */
	if (retval < 0)
		return retval;

	/* Display the port MAC address. */
	struct rte_ether_addr addr;
	retval = rte_eth_macaddr_get(port, &addr);
	if (retval != 0)
		return retval;

	printf("Port %u MAC: %02" PRIx8 " %02" PRIx8 " %02" PRIx8
			   " %02" PRIx8 " %02" PRIx8 " %02" PRIx8 "\n",
			port, RTE_ETHER_ADDR_BYTES(&addr));

	/* Enable RX in promiscuous mode for the Ethernet device. */
	retval = rte_eth_promiscuous_enable(port);
	/* End of setting RX port in promiscuous mode. */
	if (retval != 0)
		return retval;

	return 0;
}

void dpdk_init(cfg_t cfg, int argc, char *argv[]) {
	if (rte_eal_init(argc, argv) < 0)
		rte_exit(EXIT_FAILURE, "Error with EAL initialization\n");
	struct rte_mempool *mbuf_pool;
	uint16_t portid;
	unsigned nb_ports = rte_eth_dev_count_avail();
	if (nb_ports == 0)
		rte_exit(EXIT_FAILURE, "Error: no ports available to program!\n");

    uint16_t FULL_BUF_SIZE = RTE_MBUF_DEFAULT_BUF_SIZE + cfg.metadata_size;

	mbuf_pool = rte_pktmbuf_pool_create("MBUF_POOL", cfg.num_mbufs * nb_ports,
		cfg.mbuf_cache_size, 0, FULL_BUF_SIZE, rte_socket_id());
	if (mbuf_pool == NULL) {
		int required_mem = (cfg.num_mbufs * (2048 + sizeof(struct rte_mbuf))) * nb_ports + (cfg.mbuf_cache_size * sizeof(struct rte_mbuf) * 1);
		rte_exit(EXIT_FAILURE, "Cannot create mbuf pool. the memory required was: %i\n", required_mem);
	}

	RTE_ETH_FOREACH_DEV(portid)
		if (port_init(cfg, portid, mbuf_pool) != 0)
			rte_exit(EXIT_FAILURE, "Cannot init port %"PRIu16 "\n",
					portid);
	return;
}

