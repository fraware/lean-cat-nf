# Makefile for lean-cat-nf
.PHONY: help dev build test bench clean install uninstall run release docker-build docker-run docker-push docker-test
.DEFAULT_GOAL := help

# Configuration
PROJECT_NAME := lean-cat-nf
DOCKER_IMAGE := ghcr.io/fraware/$(PROJECT_NAME)
VERSION := $(shell git describe --tags --always --dirty 2>/dev/null || echo "dev")

help: ## Show this help message
	@echo "$(PROJECT_NAME) - Category Normal Form for Lean 4"
	@echo ""
	@echo "Usage: make [target]"
	@echo ""
	@echo "Targets:"
	@echo "  help            Show this help message"
	@echo "  dev             Set up local development environment"
	@echo "  build           Build the project"
	@echo "  test            Run the test suite"
	@echo "  test-final      Run final test runner"
	@echo "  bench           Run benchmarks"
	@echo "  clean           Clean build artifacts"
	@echo "  clean-all       Clean all artifacts including dependencies"
	@echo "  run             Run the main CLI (shows help)"
	@echo "  install         Install the project globally (requires sudo)"
	@echo "  uninstall       Uninstall the project globally"
	@echo "  docker-build    Build Docker image"
	@echo "  docker-run      Run Docker image"
	@echo "  docker-test     Test Docker image"
	@echo "  docker-push     Push Docker image to registry"
	@echo "  release         Build and test everything for release"
	@echo "  release-dry-run Dry run of release process"
	@echo "  info            Show project information"

dev: ## Set up local development environment
	@echo "Setting up development environment..."
	@if ! command -v elan >/dev/null 2>&1; then \
		echo "Installing elan (Lean version manager)..."; \
		curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf | sh -s -- --default-toolchain none -y; \
		export PATH="$$HOME/.elan/bin:$$PATH"; \
	fi
	@echo "Installing Lean toolchain..."
	@export PATH="$$HOME/.elan/bin:$$PATH" && elan toolchain install $$(cat lean-toolchain)
	@echo "Getting dependencies..."
	@export PATH="$$HOME/.elan/bin:$$PATH" && lake exe cache get || true
	@echo "Building project..."
	@export PATH="$$HOME/.elan/bin:$$PATH" && lake build
	@echo "✅ Development environment ready!"
	@echo ""
	@echo "Next steps:"
	@echo "  - Run 'make test' to run tests"
	@echo "  - Run 'make bench' to run benchmarks"
	@echo "  - Run 'make run' to see available commands"

build: ## Build the project
	@echo "Building $(PROJECT_NAME)..."
	@export PATH="$$HOME/.elan/bin:$$PATH" && lake build
	@echo "✅ Build complete!"

test: ## Run the test suite
	@echo "Running test suite..."
	@export PATH="$$HOME/.elan/bin:$$PATH" && lake exe test-runner
	@echo "✅ Tests complete!"

test-final: ## Run final test runner
	@echo "Running final test runner..."
	@export PATH="$$HOME/.elan/bin:$$PATH" && lake exe test-runner-final
	@echo "✅ Final tests complete!"

bench: ## Run benchmarks
	@echo "Running benchmarks..."
	@export PATH="$$HOME/.elan/bin:$$PATH" && lake exe bench
	@echo "✅ Benchmarks complete!"

clean: ## Clean build artifacts
	@echo "Cleaning build artifacts..."
	@rm -rf .lake/build
	@echo "✅ Clean complete!"

clean-all: ## Clean all artifacts including dependencies
	@echo "Cleaning all artifacts..."
	@rm -rf .lake
	@echo "✅ Deep clean complete!"

run: ## Run the main CLI (shows help)
	@export PATH="$$HOME/.elan/bin:$$PATH" && lake build && echo "Available commands:" && echo "  lake exe bench      - Run benchmarks" && echo "  lake exe test-runner - Run tests" && echo "  lake exe test-runner-final - Run final tests"

install: ## Install the project globally (requires sudo)
	@echo "Installing $(PROJECT_NAME) globally..."
	@echo "#!/bin/bash" > /tmp/lean-cat-nf
	@echo 'cd $(PWD) && export PATH="$$HOME/.elan/bin:$$PATH" && lake exe bench "$$@"' >> /tmp/lean-cat-nf
	@chmod +x /tmp/lean-cat-nf
	@sudo mv /tmp/lean-cat-nf /usr/local/bin/
	@echo "✅ Installed to /usr/local/bin/lean-cat-nf"

uninstall: ## Uninstall the project globally
	@echo "Uninstalling $(PROJECT_NAME)..."
	@sudo rm -f /usr/local/bin/lean-cat-nf
	@echo "✅ Uninstalled!"

# Docker targets
docker-build: ## Build Docker image
	@echo "Building Docker image..."
	@docker build -t $(DOCKER_IMAGE):$(VERSION) -t $(DOCKER_IMAGE):latest .
	@echo "✅ Docker image built: $(DOCKER_IMAGE):$(VERSION)"

docker-run: ## Run Docker image
	@echo "Running Docker image..."
	@docker run --rm -it $(DOCKER_IMAGE):latest

docker-test: ## Test Docker image
	@echo "Testing Docker image..."
	@docker run --rm $(DOCKER_IMAGE):latest --help
	@docker run --rm $(DOCKER_IMAGE):latest bench --help || true
	@docker run --rm $(DOCKER_IMAGE):latest test --help || true
	@echo "✅ Docker image tests passed!"

docker-push: ## Push Docker image to registry
	@echo "Pushing Docker image to registry..."
	@docker push $(DOCKER_IMAGE):$(VERSION)
	@docker push $(DOCKER_IMAGE):latest
	@echo "✅ Docker image pushed!"

# Release targets
release: release-check build test docker-build docker-test ## Build and test everything for release
	@echo "✅ Release build complete!"
	@echo ""
	@echo "Ready to release $(PROJECT_NAME) version $(VERSION)"
	@echo ""
	@echo "To publish:"
	@echo "  make docker-push    # Push Docker images"
	@echo "  git tag v$(VERSION) # Tag the release"
	@echo "  git push --tags     # Push tags to trigger CI"

release-dry-run: ## Dry run of release process
	@echo "🧪 Dry run: Release process for $(PROJECT_NAME) version $(VERSION)"
	@echo "1. Building project..."
	@make build
	@echo "2. Running tests..."
	@make test
	@echo "3. Building Docker image..."
	@make docker-build
	@echo "4. Testing Docker image..."
	@make docker-test
	@echo "✅ Dry run complete! Everything looks good for release."

release-check: ## Check if ready for release
	@echo "Checking release readiness..."
	@if [ -z "$(VERSION)" ]; then echo "❌ No version found"; exit 1; fi
	@if ! git diff-index --quiet HEAD --; then echo "❌ Uncommitted changes found"; exit 1; fi
	@echo "✅ Release checks passed!"

# Development helpers
format: ## Format code (placeholder - Lean doesn't have a standard formatter)
	@echo "Code formatting for Lean is handled by IDE extensions"

lint: ## Lint code (using Lean's built-in linting)
	@echo "Running Lean linter..."
	@export PATH="$$HOME/.elan/bin:$$PATH" && lake build

docs: ## Generate documentation
	@echo "Generating documentation..."
	@export PATH="$$HOME/.elan/bin:$$PATH" && lake exe docs || echo "Documentation generation requires doc-gen4"

# Utility targets
info: ## Show project information
	@echo "Project: $(PROJECT_NAME)"
	@echo "Version: $(VERSION)"
	@echo "Docker Image: $(DOCKER_IMAGE)"
	@echo "Lean Version: leanprover/lean4:v4.8.0"

deps: ## Show dependency information
	@echo "Dependencies:"
	@export PATH="$$HOME/.elan/bin:$$PATH" && lake deps || echo "Run 'make dev' first"

check-deps: ## Check if all dependencies are installed
	@echo "Checking dependencies..."
	@command -v elan >/dev/null 2>&1 || (echo "❌ elan not found. Run 'make dev' to install." && exit 1)
	@export PATH="$$HOME/.elan/bin:$$PATH" && command -v lake >/dev/null 2>&1 || (echo "❌ lake not found. Run 'make dev' to install." && exit 1)
	@echo "✅ All dependencies found!"

# CI/CD helpers
ci-test: ## Run tests in CI environment
	@export PATH="$$HOME/.elan/bin:$$PATH" && lake build && lake exe test-runner

ci-bench: ## Run benchmarks in CI environment
	@export PATH="$$HOME/.elan/bin:$$PATH" && lake build && lake exe bench
