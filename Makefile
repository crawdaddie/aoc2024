include .env
YEAR := 2024
.PHONY: generate generate_day exec all all_time

padded_day = $(shell printf '%02d' $(DAY))


bin/day%.ml:
	@if [ -z "$(DAY)" ]; then \
		echo "Error: DAY parameter is required. Usage: make DAY=<number> fetch_input"; \
		exit 1; \
	fi
	@mkdir -p bin/inputs
	@echo "Generating script from template for day $(padded_day)..."
	@DAY=day$(padded_day) envsubst < bin/template.ml > bin/day$(padded_day).ml
	@echo "Created bin/day$(padded_day).ml"
	@echo '\
	(executable\
	\n (public_name day$(padded_day))\
	\n (name day$(padded_day))\
	\n (libraries util))\
	\n' >> bin/dune


bin/inputs/day%.txt:
	@if [ -z "$(DAY)" ]; then \
		echo "Error: DAY parameter is required. Usage: make DAY=<number> fetch_input"; \
		exit 1; \
	fi
	@mkdir -p bin/inputs
	@echo "Fetching input for day $(padded_day)..."
	@curl -s 'https://adventofcode.com/$(YEAR)/day/$(DAY)/input' \
		-H 'accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7' \
		-H 'accept-language: en-US,en;q=0.9' \
		-H 'cache-control: max-age=0' \
		-H 'cookie: $(SESSION_COOKIE)' \
		-H 'priority: u=0, i' \
		-H 'referer: https://adventofcode.com/$(YEAR)/day/$(DAY)' \
		-H 'sec-ch-ua: "Google Chrome";v="131", "Chromium";v="131", "Not_A Brand";v="24"' \
		-H 'sec-ch-ua-mobile: ?0' \
		-H 'sec-ch-ua-platform: "macOS"' \
		-H 'sec-fetch-dest: document' \
		-H 'sec-fetch-mode: navigate' \
		-H 'sec-fetch-site: same-origin' \
		-H 'sec-fetch-user: ?1' \
		-H 'upgrade-insecure-requests: 1' \
		-H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36' \
		> bin/inputs/day$(padded_day).txt
	@echo "Fetched input to bin/inputs/day$(padded_day).txt"

fetch_input: bin/inputs/day$(padded_day).txt


generate: bin/inputs/day$(padded_day).txt bin/day$(padded_day).ml

exec:
	@if [ -z "$(DAY)" ]; then \
		echo "Error: DAY parameter is required. Usage: make DAY=<number> fetch_input"; \
		exit 1; \
	fi
	dune exec day$(padded_day)

DAYS := $(sort $(patsubst bin/day%.ml,%,$(wildcard bin/day*.ml)))
all:
	@echo "Running all available solutions..."
	@for day in $(DAYS); do \
		echo "\nExecuting day $$day solution:"; \
		dune exec day$$day --profile release || exit 1; \
	done

all_time:
	@echo "Running all available solutions..."
	@total_start=$$(date +%s.%N); \
	for day in $(DAYS); do \
		printf "\nExecuting day $$day solution:\n"; \
		start=$$(date +%s.%N); \
		dune exec day$$day || exit 1; \
		end=$$(date +%s.%N); \
		runtime=$$(echo "$$end - $$start" | bc); \
		printf "Day $$day completed in %0.5f seconds\n" "$$runtime"; \
	done; \
	total_end=$$(date +%s.%N); \
	total_runtime=$$(echo "$$total_end - $$total_start" | bc); \
	printf "\nTotal runtime: %0.3f seconds\n" "$$total_runtime"
