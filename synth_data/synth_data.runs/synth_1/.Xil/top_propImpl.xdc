set_property SRC_FILE_INFO {cfile:{C:/Users/archi/OneDrive - University of Bristol/Vivado_Projects/Assignment 2/uart_demo/Cmod-A7-Master.xdc} rfile:{../../../../../../../OneDrive - University of Bristol/Vivado_Projects/Assignment 2/uart_demo/Cmod-A7-Master.xdc} id:1} [current_design]
set_property src_info {type:XDC file:1 line:7 export:INPUT save:INPUT read:READ} [current_design]
set_property -dict { PACKAGE_PIN L17   IOSTANDARD LVCMOS33 } [get_ports { clk }]; #IO_L12P_T1_MRCC_14 Sch=gclk
set_property src_info {type:XDC file:1 line:20 export:INPUT save:INPUT read:READ} [current_design]
set_property -dict { PACKAGE_PIN A18   IOSTANDARD LVCMOS33 } [get_ports { reset }]; #IO_L19N_T3_VREF_16 Sch=btn[0]
set_property src_info {type:XDC file:1 line:21 export:INPUT save:INPUT read:READ} [current_design]
set_property -dict { PACKAGE_PIN B18   IOSTANDARD LVCMOS33 } [get_ports { clear }]; #IO_L19P_T3_16 Sch=btn[1]
set_property src_info {type:XDC file:1 line:88 export:INPUT save:INPUT read:READ} [current_design]
set_property -dict { PACKAGE_PIN J18   IOSTANDARD LVCMOS33 } [get_ports { txData }]; #IO_L7N_T1_D10_14 Sch=uart_rxd_out
set_property src_info {type:XDC file:1 line:89 export:INPUT save:INPUT read:READ} [current_design]
set_property -dict { PACKAGE_PIN J17   IOSTANDARD LVCMOS33 } [get_ports { rxData  }]; #IO_L7P_T1_D09_14 Sch=uart_txd_in
