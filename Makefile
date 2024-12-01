include .env
YEAR := 2024
.PHONY: generate generate_day exec

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
		-H 'accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8' \
		-H 'accept-language: en-GB,en;q=0.8' \
		-H 'cache-control: no-cache' \
		-H 'cookie: $(SESSION_COOKIE)' \
		-H 'pragma: no-cache' \
		-H 'priority: u=0, i' \
		-H 'referer: https://adventofcode.com/$(YEAR)/day/$(DAY)' \
		-H 'sec-ch-ua: "Chromium";v="130", "Brave";v="130", "Not?A_Brand";v="99"' \
		-H 'sec-ch-ua-mobile: ?0' \
		-H 'sec-ch-ua-platform: "macOS"' \
		-H 'sec-fetch-dest: document' \
		-H 'sec-fetch-mode: navigate' \
		-H 'sec-fetch-site: same-origin' \
		-H 'sec-fetch-user: ?1' \
		-H 'sec-gpc: 1' \
		-H 'upgrade-insecure-requests: 1' \
		-H 'user-agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36' \
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
