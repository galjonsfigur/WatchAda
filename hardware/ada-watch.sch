EESchema Schematic File Version 5
EELAYER 30 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 1
Title "Ada-powereded digital watch"
Date "2019-10-04"
Rev "1"
Comp "Galion"
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
Comment5 ""
Comment6 ""
Comment7 ""
Comment8 ""
Comment9 ""
$EndDescr
$Comp
L custom_symbols:SW_Push_SKRPACE101 SW1
U 1 1 5D8513AD
P 7550 5025
F 0 "SW1" V 7675 5025 50  0000 C CNN
F 1 "SW_Push" V 7375 5025 50  0000 C CNN
F 2 "custom_symbols:SKRPACE010" H 7550 5225 50  0001 C CNN
F 3 "~" H 7550 5225 50  0001 C CNN
	1    7550 5025
	0    1    1    0   
$EndComp
$Comp
L custom_symbols:HP5082-7414 U1
U 1 1 5D8D7FBD
P 6000 1500
F 0 "U1" H 6000 2167 50  0000 C CNN
F 1 "HP5082-7414" H 6000 2076 50  0000 C CNN
F 2 "custom_symbols:DIP-12_W7.62mm_CustomPads_NoSilk" H 6000 900 50  0001 C CNN
F 3 "https://archive.org/details/bitsavers_hpdataBookicsDesignersCatalog_47988337/page/n71" H 5570 1530 50  0001 C CNN
	1    6000 1500
	1    0    0    -1  
$EndComp
$Comp
L Device:R_Small R8
U 1 1 5D8E0FE9
P 4700 1900
F 0 "R8" V 4700 675 50  0000 C CNN
F 1 "330" V 4595 1900 50  0000 C CNN
F 2 "Resistor_SMD:R_0603_1608Metric_Pad1.05x0.95mm_HandSolder" H 4700 1900 50  0001 C CNN
F 3 "~" H 4700 1900 50  0001 C CNN
	1    4700 1900
	0    1    1    0   
$EndComp
$Comp
L Device:R_Small R4
U 1 1 5D8E14E3
P 4425 1800
F 0 "R4" V 4425 850 50  0000 C CNN
F 1 "330" V 4320 1800 50  0000 C CNN
F 2 "Resistor_SMD:R_0603_1608Metric_Pad1.05x0.95mm_HandSolder" H 4425 1800 50  0001 C CNN
F 3 "~" H 4425 1800 50  0001 C CNN
	1    4425 1800
	0    1    1    0   
$EndComp
$Comp
L Device:R_Small R7
U 1 1 5D8E17AB
P 4700 1700
F 0 "R7" V 4700 475 50  0000 C CNN
F 1 "330" V 4595 1700 50  0000 C CNN
F 2 "Resistor_SMD:R_0603_1608Metric_Pad1.05x0.95mm_HandSolder" H 4700 1700 50  0001 C CNN
F 3 "~" H 4700 1700 50  0001 C CNN
	1    4700 1700
	0    1    1    0   
$EndComp
$Comp
L Device:R_Small R3
U 1 1 5D8E18D3
P 4425 1600
F 0 "R3" V 4425 750 50  0000 C CNN
F 1 "330" V 4320 1600 50  0000 C CNN
F 2 "Resistor_SMD:R_0603_1608Metric_Pad1.05x0.95mm_HandSolder" H 4425 1600 50  0001 C CNN
F 3 "~" H 4425 1600 50  0001 C CNN
	1    4425 1600
	0    1    1    0   
$EndComp
$Comp
L Device:R_Small R6
U 1 1 5D8E1A3B
P 4700 1500
F 0 "R6" V 4700 475 50  0000 C CNN
F 1 "330" V 4595 1500 50  0000 C CNN
F 2 "Resistor_SMD:R_0603_1608Metric_Pad1.05x0.95mm_HandSolder" H 4700 1500 50  0001 C CNN
F 3 "~" H 4700 1500 50  0001 C CNN
	1    4700 1500
	0    1    1    0   
$EndComp
$Comp
L Device:R_Small R2
U 1 1 5D8E1B33
P 4425 1400
F 0 "R2" V 4425 750 50  0000 C CNN
F 1 "330" V 4320 1400 50  0000 C CNN
F 2 "Resistor_SMD:R_0603_1608Metric_Pad1.05x0.95mm_HandSolder" H 4425 1400 50  0001 C CNN
F 3 "~" H 4425 1400 50  0001 C CNN
	1    4425 1400
	0    1    1    0   
$EndComp
$Comp
L Device:R_Small R5
U 1 1 5D8E3166
P 4700 1300
F 0 "R5" V 4700 475 50  0000 C CNN
F 1 "330" V 4600 1300 50  0000 C CNN
F 2 "Resistor_SMD:R_0603_1608Metric_Pad1.05x0.95mm_HandSolder" H 4700 1300 50  0001 C CNN
F 3 "~" H 4700 1300 50  0001 C CNN
	1    4700 1300
	0    1    1    0   
$EndComp
$Comp
L Device:R_Small R1
U 1 1 5D8E33AE
P 4425 1200
F 0 "R1" V 4425 750 50  0000 C CNN
F 1 "330" V 4320 1200 50  0000 C CNN
F 2 "Resistor_SMD:R_0603_1608Metric_Pad1.05x0.95mm_HandSolder" H 4425 1200 50  0001 C CNN
F 3 "~" H 4425 1200 50  0001 C CNN
	1    4425 1200
	0    1    1    0   
$EndComp
Wire Wire Line
	4900 1200 4525 1200
Wire Wire Line
	4900 1300 4800 1300
Wire Wire Line
	4900 1400 4525 1400
Wire Wire Line
	4900 1500 4800 1500
Wire Wire Line
	4900 1600 4525 1600
Wire Wire Line
	4900 1700 4800 1700
Wire Wire Line
	4900 1800 4525 1800
Wire Wire Line
	4900 1900 4800 1900
Wire Wire Line
	4300 3700 4075 3700
Wire Wire Line
	4075 3700 4075 1200
Wire Wire Line
	4075 1200 4325 1200
Wire Wire Line
	4600 1300 3975 1300
Wire Wire Line
	3975 1300 3975 3800
Wire Wire Line
	3975 3800 4300 3800
Wire Wire Line
	4325 1400 3875 1400
Wire Wire Line
	3875 1400 3875 3900
Wire Wire Line
	3875 3900 4300 3900
Wire Wire Line
	4600 1500 3775 1500
Wire Wire Line
	3775 1500 3775 4000
Wire Wire Line
	3775 4000 4300 4000
Wire Wire Line
	4325 1600 3675 1600
Wire Wire Line
	3675 1600 3675 4100
Wire Wire Line
	3675 4100 4300 4100
Wire Wire Line
	4600 1700 3575 1700
Wire Wire Line
	3575 1700 3575 4200
Wire Wire Line
	3575 4200 4300 4200
Wire Wire Line
	7900 4000 7700 4000
$Comp
L power:GND #PWR0101
U 1 1 5D8E6EBF
P 5850 5125
F 0 "#PWR0101" H 5850 4875 50  0001 C CNN
F 1 "GND" H 5675 5125 50  0000 C CNN
F 2 "" H 5850 5125 50  0001 C CNN
F 3 "" H 5850 5125 50  0001 C CNN
	1    5850 5125
	1    0    0    -1  
$EndComp
Wire Wire Line
	8000 4300 8000 5025
Wire Wire Line
	6000 5025 6000 4750
$Comp
L custom_symbols:KDT00030 Q5
U 1 1 5D8E8C16
P 9100 3900
F 0 "Q5" H 9290 3946 50  0000 L CNN
F 1 "KDT00030" H 9290 3855 50  0000 L CNN
F 2 "LED_SMD:LED_0603_1608Metric" H 9580 3760 50  0001 C CNN
F 3 "https://www.onsemi.com/pub/Collateral/KDT00030-D.PDF" H 9100 3900 50  0001 C CNN
	1    9100 3900
	1    0    0    -1  
$EndComp
Wire Wire Line
	7700 4200 9200 4200
Wire Wire Line
	9200 4200 9200 4100
$Comp
L power:GND #PWR0102
U 1 1 5D8EB810
P 9200 4700
F 0 "#PWR0102" H 9200 4450 50  0001 C CNN
F 1 "GND" H 9205 4527 50  0000 C CNN
F 2 "" H 9200 4700 50  0001 C CNN
F 3 "" H 9200 4700 50  0001 C CNN
	1    9200 4700
	1    0    0    -1  
$EndComp
$Comp
L Device:R_Small R13
U 1 1 5D8EBB73
P 9200 4450
F 0 "R13" H 9259 4404 50  0000 L CNN
F 1 "10k" H 9259 4495 50  0000 L CNN
F 2 "Resistor_SMD:R_0603_1608Metric_Pad1.05x0.95mm_HandSolder" H 9200 4450 50  0001 C CNN
F 3 "~" H 9200 4450 50  0001 C CNN
	1    9200 4450
	-1   0    0    1   
$EndComp
Wire Wire Line
	9200 4350 9200 4200
Connection ~ 9200 4200
Wire Wire Line
	9200 4700 9200 4550
$Comp
L Device:Battery_Cell BT1
U 1 1 5D8EC008
P 2575 4025
F 0 "BT1" H 2693 4121 50  0000 L CNN
F 1 "CR2032" H 2693 4030 50  0000 L CNN
F 2 "custom_symbols:BatteryHolder_Pinrex_780-72-00GF51_1x2032_customSilk" V 2575 4085 50  0001 C CNN
F 3 "~" V 2575 4085 50  0001 C CNN
	1    2575 4025
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0103
U 1 1 5D8F40FF
P 2575 4225
F 0 "#PWR0103" H 2575 3975 50  0001 C CNN
F 1 "GND" H 2580 4052 50  0000 C CNN
F 2 "" H 2575 4225 50  0001 C CNN
F 3 "" H 2575 4225 50  0001 C CNN
	1    2575 4225
	1    0    0    -1  
$EndComp
$Comp
L power:+3V0 #PWR0104
U 1 1 5D8F4351
P 2575 3725
F 0 "#PWR0104" H 2575 3575 50  0001 C CNN
F 1 "+3V0" H 2590 3898 50  0000 C CNN
F 2 "" H 2575 3725 50  0001 C CNN
F 3 "" H 2575 3725 50  0001 C CNN
	1    2575 3725
	1    0    0    -1  
$EndComp
$Comp
L power:+3V0 #PWR0105
U 1 1 5D8F4773
P 6000 3075
F 0 "#PWR0105" H 6000 2925 50  0001 C CNN
F 1 "+3V0" H 5875 3075 50  0000 C CNN
F 2 "" H 6000 3075 50  0001 C CNN
F 3 "" H 6000 3075 50  0001 C CNN
	1    6000 3075
	1    0    0    -1  
$EndComp
Wire Wire Line
	6000 3075 6000 3150
$Comp
L power:+3V0 #PWR0106
U 1 1 5D8F4C14
P 9200 3600
F 0 "#PWR0106" H 9200 3450 50  0001 C CNN
F 1 "+3V0" H 9215 3773 50  0000 C CNN
F 2 "" H 9200 3600 50  0001 C CNN
F 3 "" H 9200 3600 50  0001 C CNN
	1    9200 3600
	1    0    0    -1  
$EndComp
Wire Wire Line
	9200 3600 9200 3700
Wire Wire Line
	4325 1800 4175 1800
Wire Wire Line
	4175 2875 7900 2875
Wire Wire Line
	4275 1900 4600 1900
$Comp
L Transistor_BJT:MMBT3904 Q4
U 1 1 5D9BC964
P 7500 2175
F 0 "Q4" H 7691 2221 50  0000 L CNN
F 1 "MMBT3904" H 7200 2400 50  0000 L CNN
F 2 "Package_TO_SOT_SMD:SOT-23" H 7700 2100 50  0001 L CIN
F 3 "https://www.fairchildsemi.com/datasheets/2N/2N3904.pdf" H 7500 2175 50  0001 L CNN
	1    7500 2175
	-1   0    0    -1  
$EndComp
$Comp
L Transistor_BJT:MMBT3904 Q3
U 1 1 5D9C27D2
P 8100 2175
F 0 "Q3" H 8291 2221 50  0000 L CNN
F 1 "MMBT3904" H 7800 2400 50  0000 L CNN
F 2 "Package_TO_SOT_SMD:SOT-23" H 8300 2100 50  0001 L CIN
F 3 "https://www.fairchildsemi.com/datasheets/2N/2N3904.pdf" H 8100 2175 50  0001 L CNN
	1    8100 2175
	-1   0    0    -1  
$EndComp
$Comp
L Transistor_BJT:MMBT3904 Q1
U 1 1 5D9C71AD
P 9300 2175
F 0 "Q1" H 9491 2221 50  0000 L CNN
F 1 "MMBT3904" H 9000 2400 50  0000 L CNN
F 2 "Package_TO_SOT_SMD:SOT-23" H 9500 2100 50  0001 L CIN
F 3 "https://www.fairchildsemi.com/datasheets/2N/2N3904.pdf" H 9300 2175 50  0001 L CNN
	1    9300 2175
	-1   0    0    -1  
$EndComp
$Comp
L Transistor_BJT:MMBT3904 Q2
U 1 1 5D9C71BA
P 8700 2175
F 0 "Q2" H 8891 2221 50  0000 L CNN
F 1 "MMBT3904" H 8400 2400 50  0000 L CNN
F 2 "Package_TO_SOT_SMD:SOT-23" H 8900 2100 50  0001 L CIN
F 3 "https://www.fairchildsemi.com/datasheets/2N/2N3904.pdf" H 8700 2175 50  0001 L CNN
	1    8700 2175
	-1   0    0    -1  
$EndComp
Wire Wire Line
	7100 1900 7400 1900
Wire Wire Line
	7400 1900 7400 1975
Wire Wire Line
	7100 1800 8000 1800
Wire Wire Line
	8000 1800 8000 1975
Wire Wire Line
	8600 1700 8600 1975
Wire Wire Line
	9200 1600 9200 1975
$Comp
L Device:R_Small R10
U 1 1 5D9C871D
P 7800 2375
F 0 "R10" V 7875 2375 50  0000 C CNN
F 1 "10k" V 7725 2375 50  0000 C CNN
F 2 "Resistor_SMD:R_0603_1608Metric_Pad1.05x0.95mm_HandSolder" H 7800 2375 50  0001 C CNN
F 3 "~" H 7800 2375 50  0001 C CNN
	1    7800 2375
	-1   0    0    1   
$EndComp
$Comp
L Device:R_Small R11
U 1 1 5D9C8C29
P 8400 2375
F 0 "R11" V 8475 2375 50  0000 C CNN
F 1 "10k" V 8325 2375 50  0000 C CNN
F 2 "Resistor_SMD:R_0603_1608Metric_Pad1.05x0.95mm_HandSolder" H 8400 2375 50  0001 C CNN
F 3 "~" H 8400 2375 50  0001 C CNN
	1    8400 2375
	-1   0    0    1   
$EndComp
$Comp
L Device:R_Small R12
U 1 1 5D9C8D61
P 9000 2375
F 0 "R12" V 9075 2375 50  0000 C CNN
F 1 "10k" V 8925 2375 50  0000 C CNN
F 2 "Resistor_SMD:R_0603_1608Metric_Pad1.05x0.95mm_HandSolder" H 9000 2375 50  0001 C CNN
F 3 "~" H 9000 2375 50  0001 C CNN
	1    9000 2375
	-1   0    0    1   
$EndComp
$Comp
L Device:R_Small R14
U 1 1 5D9C9A3B
P 9600 2375
F 0 "R14" V 9675 2375 50  0000 C CNN
F 1 "10k" V 9525 2375 50  0000 C CNN
F 2 "Resistor_SMD:R_0603_1608Metric_Pad1.05x0.95mm_HandSolder" H 9600 2375 50  0001 C CNN
F 3 "~" H 9600 2375 50  0001 C CNN
	1    9600 2375
	-1   0    0    1   
$EndComp
Wire Wire Line
	7700 2175 7800 2175
Wire Wire Line
	7800 2175 7800 2275
Wire Wire Line
	8300 2175 8400 2175
Wire Wire Line
	8400 2175 8400 2275
Wire Wire Line
	8900 2175 9000 2175
Wire Wire Line
	9000 2175 9000 2275
Wire Wire Line
	9600 2175 9600 2275
Wire Wire Line
	9500 2175 9600 2175
Wire Wire Line
	7100 1700 8600 1700
Wire Wire Line
	7100 1600 9200 1600
Wire Wire Line
	8400 2475 8400 2575
Wire Wire Line
	8400 2575 8650 2575
Wire Wire Line
	8650 2575 8650 3800
Wire Wire Line
	8650 3800 7700 3800
Wire Wire Line
	9000 2475 9000 2575
Wire Wire Line
	9000 2575 8750 2575
Wire Wire Line
	8750 2575 8750 3700
Wire Wire Line
	8750 3700 7700 3700
Wire Wire Line
	9600 2475 9600 2675
Wire Wire Line
	9600 2675 8850 2675
Wire Wire Line
	8850 2675 8850 3600
Wire Wire Line
	8850 3600 7700 3600
Wire Wire Line
	4275 2775 8000 2775
Wire Wire Line
	7700 4100 8000 4100
Wire Wire Line
	7800 2475 7800 2675
Wire Wire Line
	7800 2675 8550 2675
Wire Wire Line
	8550 2675 8550 3900
Wire Wire Line
	8550 3900 7700 3900
Wire Wire Line
	7900 2875 7900 4000
Wire Wire Line
	8000 2775 8000 4100
Wire Wire Line
	4275 1900 4275 2775
Wire Wire Line
	4175 1800 4175 2875
$Comp
L power:GND #PWR0107
U 1 1 5D9BDC07
P 7400 2450
F 0 "#PWR0107" H 7400 2200 50  0001 C CNN
F 1 "GND" H 7525 2450 50  0000 C CNN
F 2 "" H 7400 2450 50  0001 C CNN
F 3 "" H 7400 2450 50  0001 C CNN
	1    7400 2450
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0108
U 1 1 5D9BDFFE
P 8000 2450
F 0 "#PWR0108" H 8000 2200 50  0001 C CNN
F 1 "GND" H 8125 2450 50  0000 C CNN
F 2 "" H 8000 2450 50  0001 C CNN
F 3 "" H 8000 2450 50  0001 C CNN
	1    8000 2450
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0109
U 1 1 5D9BE890
P 8600 2450
F 0 "#PWR0109" H 8600 2200 50  0001 C CNN
F 1 "GND" H 8725 2450 50  0000 C CNN
F 2 "" H 8600 2450 50  0001 C CNN
F 3 "" H 8600 2450 50  0001 C CNN
	1    8600 2450
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0110
U 1 1 5D9BEBA3
P 9200 2450
F 0 "#PWR0110" H 9200 2200 50  0001 C CNN
F 1 "GND" H 9325 2450 50  0000 C CNN
F 2 "" H 9200 2450 50  0001 C CNN
F 3 "" H 9200 2450 50  0001 C CNN
	1    9200 2450
	1    0    0    -1  
$EndComp
Wire Wire Line
	9200 2450 9200 2375
Wire Wire Line
	8600 2450 8600 2375
Wire Wire Line
	8000 2450 8000 2375
Wire Wire Line
	7400 2375 7400 2450
$Comp
L Connector_Generic:Conn_02x04_Counter_Clockwise J1
U 1 1 5D9C13E9
P 5950 5625
F 0 "J1" V 5954 5337 50  0000 R CNN
F 1 "SOICBite" V 6045 5337 50  0000 R CNN
F 2 "custom_symbols:SOIC_clipProgSmall" H 5950 5625 50  0001 C CNN
F 3 "~" H 5950 5625 50  0001 C CNN
	1    5950 5625
	0    -1   1    0   
$EndComp
Wire Wire Line
	4300 3550 4200 3550
Wire Wire Line
	4200 3550 4200 5025
Wire Wire Line
	5850 5425 5850 5325
Wire Wire Line
	4200 5325 5850 5325
Wire Wire Line
	7700 3450 8100 3450
Wire Wire Line
	8100 3450 8100 5325
Wire Wire Line
	6150 5325 6150 5425
Wire Wire Line
	7700 4300 8000 4300
Wire Wire Line
	5950 5425 5950 5350
Wire Wire Line
	6050 5350 6050 5425
Wire Wire Line
	6000 5350 6000 5025
Connection ~ 6000 5025
Wire Wire Line
	5950 5350 6000 5350
Wire Wire Line
	5850 5125 5850 5025
Wire Wire Line
	5850 5025 6000 5025
Connection ~ 6000 5350
Wire Wire Line
	6000 5350 6050 5350
Wire Wire Line
	6150 5325 8100 5325
$Comp
L Device:R_Small R9
U 1 1 5D9C5C32
P 5150 3150
F 0 "R9" V 5225 3100 50  0000 L CNN
F 1 "47k" V 5075 3075 50  0000 L CNN
F 2 "Resistor_SMD:R_0603_1608Metric" H 5150 3150 50  0001 C CNN
F 3 "~" H 5150 3150 50  0001 C CNN
	1    5150 3150
	0    -1   -1   0   
$EndComp
$Comp
L MCU_Texas_MSP430:MSP430G2553IPW20 U2
U 1 1 5D84E2F0
P 6000 4000
F 0 "U2" H 5425 4700 50  0000 C CNN
F 1 "MSP430G2553IPW20" H 4850 4700 50  0000 C CNN
F 2 "Package_SO:TSSOP-20_4.4x6.5mm_P0.65mm" H 4550 3450 50  0001 C CIN
F 3 "http://www.ti.com/lit/ds/symlink/msp430g2553.pdf" H 5950 4000 50  0001 C CNN
	1    6000 4000
	1    0    0    -1  
$EndComp
Wire Wire Line
	6000 3150 5250 3150
Connection ~ 6000 3150
Wire Wire Line
	6000 3150 6000 3200
Wire Wire Line
	5050 3150 4200 3150
Wire Wire Line
	4200 3150 4200 3550
Connection ~ 4200 3550
$Comp
L power:+3V0 #PWR0111
U 1 1 5D9C72A1
P 5600 5675
F 0 "#PWR0111" H 5600 5525 50  0001 C CNN
F 1 "+3V0" H 5615 5848 50  0000 C CNN
F 2 "" H 5600 5675 50  0001 C CNN
F 3 "" H 5600 5675 50  0001 C CNN
	1    5600 5675
	1    0    0    -1  
$EndComp
Wire Wire Line
	5850 5925 5850 6025
Wire Wire Line
	5850 6025 5600 6025
Wire Wire Line
	5600 6025 5600 5675
Wire Wire Line
	5850 6025 5950 6025
Wire Wire Line
	5950 6025 5950 5925
Connection ~ 5850 6025
$Comp
L power:GND #PWR0112
U 1 1 5D9C7F1C
P 6300 6125
F 0 "#PWR0112" H 6300 5875 50  0001 C CNN
F 1 "GND" H 6305 5952 50  0000 C CNN
F 2 "" H 6300 6125 50  0001 C CNN
F 3 "" H 6300 6125 50  0001 C CNN
	1    6300 6125
	1    0    0    -1  
$EndComp
Wire Wire Line
	6050 5925 6050 6025
Wire Wire Line
	6050 6025 6150 6025
Wire Wire Line
	6300 6025 6300 6125
Wire Wire Line
	6150 5925 6150 6025
Connection ~ 6150 6025
Wire Wire Line
	6150 6025 6300 6025
$Comp
L Device:C_Small C2
U 1 1 5D9C9C2F
P 2275 3975
F 0 "C2" H 2367 4021 50  0000 L CNN
F 1 "100nF" H 2300 3900 50  0000 L CNN
F 2 "Capacitor_SMD:C_0805_2012Metric" H 2275 3975 50  0001 C CNN
F 3 "~" H 2275 3975 50  0001 C CNN
	1    2275 3975
	-1   0    0    -1  
$EndComp
$Comp
L Device:C_Small C1
U 1 1 5D9CA0C9
P 1975 3975
F 0 "C1" H 2067 4021 50  0000 L CNN
F 1 "10uF" H 2025 3900 50  0000 L CNN
F 2 "Capacitor_SMD:C_0805_2012Metric" H 1975 3975 50  0001 C CNN
F 3 "~" H 1975 3975 50  0001 C CNN
	1    1975 3975
	-1   0    0    -1  
$EndComp
Wire Wire Line
	2575 3725 2575 3775
Wire Wire Line
	2575 4225 2575 4175
Wire Wire Line
	2275 3775 2575 3775
Connection ~ 2575 3775
Wire Wire Line
	2575 3775 2575 3825
Wire Wire Line
	2275 3775 1975 3775
Connection ~ 2275 3775
Wire Wire Line
	1975 4175 2275 4175
Connection ~ 2575 4175
Wire Wire Line
	2575 4175 2575 4125
Connection ~ 2275 4175
Wire Wire Line
	2275 4175 2575 4175
Wire Wire Line
	2275 4075 2275 4175
Wire Wire Line
	1975 4075 1975 4175
Wire Wire Line
	2275 3775 2275 3875
Wire Wire Line
	1975 3775 1975 3875
$Comp
L Device:C_Small C5
U 1 1 5D9CF307
P 5100 5025
F 0 "C5" V 5200 4975 50  0000 L CNN
F 1 "1nF" V 5000 4950 50  0000 L CNN
F 2 "Capacitor_SMD:C_0402_1005Metric" H 5100 5025 50  0001 C CNN
F 3 "~" H 5100 5025 50  0001 C CNN
	1    5100 5025
	0    -1   -1   0   
$EndComp
Wire Wire Line
	5850 5025 5200 5025
Connection ~ 5850 5025
Wire Wire Line
	5000 5025 4200 5025
Connection ~ 4200 5025
Wire Wire Line
	4200 5025 4200 5325
Wire Wire Line
	3775 4650 3775 4750
Connection ~ 3975 4650
Wire Wire Line
	3975 4650 3975 4750
Connection ~ 3775 4650
$Comp
L power:GND #PWR0113
U 1 1 5D8E6B49
P 3875 5150
F 0 "#PWR0113" H 3875 4900 50  0001 C CNN
F 1 "GND" H 3880 4977 50  0000 C CNN
F 2 "" H 3875 5150 50  0001 C CNN
F 3 "" H 3875 5150 50  0001 C CNN
	1    3875 5150
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C4
U 1 1 5D8E68CB
P 3975 4850
F 0 "C4" H 4000 4925 50  0000 L CNN
F 1 "12pF" H 4000 4775 50  0000 L CNN
F 2 "Capacitor_SMD:C_0402_1005Metric" H 3975 4850 50  0001 C CNN
F 3 "~" H 3975 4850 50  0001 C CNN
	1    3975 4850
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C3
U 1 1 5D8E64CD
P 3775 4850
F 0 "C3" H 3650 4925 50  0000 L CNN
F 1 "12pF" H 3575 4775 50  0000 L CNN
F 2 "Capacitor_SMD:C_0402_1005Metric" H 3775 4850 50  0001 C CNN
F 3 "~" H 3775 4850 50  0001 C CNN
	1    3775 4850
	1    0    0    -1  
$EndComp
Wire Wire Line
	3975 4400 3975 4650
Wire Wire Line
	3775 4300 3775 4650
$Comp
L Device:Crystal_Small Y1
U 1 1 5D8E5B7B
P 3875 4650
F 0 "Y1" H 3875 4775 50  0000 C CNN
F 1 "32768kHz" H 4250 4650 50  0000 C CNN
F 2 "Crystal:Crystal_C38-LF_D3.0mm_L8.0mm_Horizontal_1EP_style1" H 3875 4650 50  0001 C CNN
F 3 "~" H 3875 4650 50  0001 C CNN
	1    3875 4650
	-1   0    0    -1  
$EndComp
Wire Wire Line
	3775 4300 4300 4300
Wire Wire Line
	3975 4400 4300 4400
Wire Wire Line
	3975 4950 3975 5050
Wire Wire Line
	3975 5050 3875 5050
Wire Wire Line
	3875 5050 3875 5150
Wire Wire Line
	3775 4950 3775 5050
Wire Wire Line
	3775 5050 3875 5050
Connection ~ 3875 5050
$Comp
L power:PWR_FLAG #FLG0101
U 1 1 5D9CC7B6
P 2875 3725
F 0 "#FLG0101" H 2875 3800 50  0001 C CNN
F 1 "PWR_FLAG" H 3075 3725 50  0000 C CNN
F 2 "" H 2875 3725 50  0001 C CNN
F 3 "~" H 2875 3725 50  0001 C CNN
	1    2875 3725
	1    0    0    -1  
$EndComp
$Comp
L power:PWR_FLAG #FLG0102
U 1 1 5D9CEC9B
P 2875 4225
F 0 "#FLG0102" H 2875 4300 50  0001 C CNN
F 1 "PWR_FLAG" H 2675 4225 50  0000 C CNN
F 2 "" H 2875 4225 50  0001 C CNN
F 3 "~" H 2875 4225 50  0001 C CNN
	1    2875 4225
	-1   0    0    1   
$EndComp
Wire Wire Line
	2875 3725 2875 3775
Wire Wire Line
	2875 3775 2575 3775
Wire Wire Line
	2575 4175 2875 4175
Wire Wire Line
	2875 4175 2875 4225
Wire Wire Line
	7850 5025 7850 4925
Wire Wire Line
	7850 4925 7750 4925
Wire Wire Line
	7850 5025 8000 5025
Wire Wire Line
	7850 5025 7850 5125
Wire Wire Line
	7850 5125 7750 5125
Connection ~ 7850 5025
Wire Wire Line
	7350 4925 7250 4925
Wire Wire Line
	7250 4925 7250 5025
Wire Wire Line
	7250 5125 7350 5125
Wire Wire Line
	7250 5025 6000 5025
Connection ~ 7250 5025
Wire Wire Line
	7250 5025 7250 5125
$Comp
L custom_symbols:Ada_Logo_Small LG1
U 1 1 5DA8A241
P 2700 3225
F 0 "LG1" H 2898 3471 50  0000 L CNN
F 1 "Ada_Logo_Small" H 2898 3380 50  0000 L CNN
F 2 "custom_symbols:Ada_Logo_Small" H 2700 3225 50  0001 C CNN
F 3 "" H 2700 3225 50  0001 C CNN
	1    2700 3225
	1    0    0    -1  
$EndComp
$Comp
L custom_symbols:Galion_Logo LG2
U 1 1 5DA8E7F4
P 2700 2575
F 0 "LG2" H 2908 2621 50  0000 L CNN
F 1 "Galion_Logo" H 2908 2530 50  0000 L CNN
F 2 "custom_symbols:Galion_Logo_Small" H 2766 2252 50  0001 C CNN
F 3 "" H 2700 2575 50  0001 C CNN
	1    2700 2575
	1    0    0    -1  
$EndComp
$Comp
L Graphic:Logo_Open_Hardware_Small LG3
U 1 1 5DA8EE8C
P 2700 2150
F 0 "LG3" H 2700 2425 50  0001 C CNN
F 1 "Logo_Open_Hardware_Small" H 2700 1925 50  0001 C CNN
F 2 "Symbol:OSHW-Symbol_6.7x6mm_SilkScreen" H 2700 2150 50  0001 C CNN
F 3 "~" H 2700 2150 50  0001 C CNN
	1    2700 2150
	1    0    0    -1  
$EndComp
$EndSCHEMATC
