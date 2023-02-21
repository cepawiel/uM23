OUTPUT_FOLDER=build
OUTPUT_VMS=$(OUTPUT_FOLDER)/uM23.vms
OUTPUT_MAP=$(OUTPUT_FOLDER)/uM23.map

.PHONEY: all build decompile out_folder

all: build decompile
build: $(OUTPUT_VMS)
decompile: $(OUTPUT_MAP)
out_folder:
	@mkdir -p $(OUTPUT_FOLDER)

$(OUTPUT_VMS): uM23.s core.s defrag.s header.s soc.s ui.s sfr.i out_folder
	waterbear assemble $< -o $@

$(OUTPUT_MAP): $(OUTPUT_VMS) out_folder
	waterbear disassemble -a -p -t game -o $@ $<