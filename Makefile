EXECUTABLE = contrast_render_engine
RUST_SOURCES := $(wildcard src/*.rs)
SHADER_SOURCE_PATH = src/shader
SHADER_TARGET_PATH = target/shader_modules
SHADER_SOURCES := $(wildcard $(SHADER_SOURCE_PATH)/*)
SHADER_TARGETS_2 := $(SHADER_SOURCES:$(SHADER_SOURCE_PATH)/%.vert=$(SHADER_TARGET_PATH)/%_vert.spv)
SHADER_TARGETS_1 := $(SHADER_TARGETS_2:$(SHADER_SOURCE_PATH)/%.frag=$(SHADER_TARGET_PATH)/%_frag.spv)
SHADER_TARGETS := $(SHADER_TARGETS_1:$(SHADER_SOURCE_PATH)/%.comp=$(SHADER_TARGET_PATH)/%_comp.spv)
WASM_MODULE_TARGET = target/wasm32-unknown-unknown/release/examples/showcase.wasm
WASM_BINDING_TAGRET = target/wasm/$(EXECUTABLE).js
DESKTOP_TARGET = target/release/examples/showcase

all: spriv wasm desktop

spriv: $(SHADER_TARGETS)

wasm: $(WASM_BINDING_TAGRET)

desktop: $(DESKTOP_TARGET)

doc:
	cargo doc --no-deps --release

.PHONY: all spriv wasm desktop doc

$(SHADER_TARGET_PATH):
	mkdir -p $(SHADER_TARGET_PATH)

$(SHADER_TARGET_PATH)/%_vert.spv: src/shader/%.vert $(SHADER_TARGET_PATH)
	glslangValidator -S vert -V460 -o $@ $<

$(SHADER_TARGET_PATH)/%_frag.spv: src/shader/%.frag $(SHADER_TARGET_PATH)
	glslangValidator -S frag -V460 -o $@ $<

$(SHADER_TARGET_PATH)/%_comp.spv: src/shader/%.comp $(SHADER_TARGET_PATH)
	glslangValidator -S comp -V460 -o $@ $<

$(DESKTOP_TARGET): $(RUST_SOURCES) spriv
	cargo build --release --examples

$(WASM_MODULE_TARGET): $(RUST_SOURCES) spriv
	RUSTFLAGS=--cfg=web_sys_unstable_apis cargo build --release --examples --target wasm32-unknown-unknown

$(WASM_BINDING_TAGRET): $(WASM_MODULE_TARGET)
	# cargo install -f wasm-bindgen-cli --version 0.2.69
	wasm-bindgen --out-dir target/wasm --web $<
