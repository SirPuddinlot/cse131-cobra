# Compile .snek to .s (AOT compilation)
test/%.s: test/%.snek src/main.rs
	cargo run --target x86_64-apple-darwin -- -c $< test/$*.s

# Compile .s to executable
test/%.run: test/%.s runtime/start.rs
	nasm -f macho64 test/$*.s -o runtime/our_code.o
	ar rcs runtime/libour_code.a runtime/our_code.o
	rustc --target x86_64-apple-darwin -L runtime/ runtime/start.rs -o test/$*.run

# JIT execute only (no assembly file generated)
test/%.jit: test/%.snek src/main.rs
	cargo run --target x86_64-apple-darwin -- -e test/$*.snek

# Both JIT execute and generate assembly (debugging)
test/%.debug: test/%.snek src/main.rs
	cargo run --target x86_64-apple-darwin -- -g $< test/$*.s

clean:
	rm -f test/*.s test/*.run runtime/*.o runtime/*.a

# JIT execute only (no assembly file generated)
repl: 
	cargo run --target x86_64-apple-darwin -- -i 

# Convenience targets
.PHONY: clean repl