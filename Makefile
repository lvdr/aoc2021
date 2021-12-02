DAY ?= 01

MAIN        ?= day$(DAY)/main.lean
TEST_IN     ?= day$(DAY)/test.in
TEST_OUT    ?= day$(DAY)/test.out
TEST_GOLDEN ?= day$(DAY)/test.golden

run: 
	lean $(MAIN) --run

test:
	lean $(MAIN) --run $(TEST_IN) | tee $(TEST_OUT)
	diff $(TEST_OUT) $(TEST_GOLDEN)
