# Multi-stage Dockerfile for lean-cat-nf
FROM ubuntu:22.04 as builder

# Install system dependencies
RUN apt-get update && apt-get install -y \
    curl \
    git \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Install Lean 4
RUN curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf | sh -s -- --default-toolchain none -y
ENV PATH="/root/.elan/bin:${PATH}"

# Set working directory
WORKDIR /app

# Copy lean toolchain first for better caching
COPY lean-toolchain ./
RUN elan toolchain install $(cat lean-toolchain)

# Copy project files
COPY Lakefile.lean lake-manifest.json ./
COPY src/ ./src/
COPY bench/ ./bench/
COPY tests/ ./tests/
COPY test_runner_final.lean ./

# Build the project
RUN lake build

# Build executables
RUN lake exe cache get
RUN lake build bench
RUN lake build test-runner
RUN lake build test-runner-final

# Production stage
FROM ubuntu:22.04

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Install Lean 4 runtime
RUN curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf | sh -s -- --default-toolchain none -y
ENV PATH="/root/.elan/bin:${PATH}"

# Copy lean toolchain and install
COPY lean-toolchain /tmp/
RUN elan toolchain install $(cat /tmp/lean-toolchain)

# Create app directory
WORKDIR /app

# Copy built artifacts from builder stage
COPY --from=builder /app/.lake ./.lake
COPY --from=builder /app/src ./src
COPY --from=builder /app/Lakefile.lean ./
COPY --from=builder /app/lake-manifest.json ./
COPY --from=builder /app/lean-toolchain ./

# Create a wrapper script for the CLI
RUN echo '#!/bin/bash\n\
    case "$1" in\n\
    bench)\n\
    shift\n\
    exec lake exe bench "$@"\n\
    ;;\n\
    test)\n\
    shift\n\
    exec lake exe test-runner "$@"\n\
    ;;\n\
    test-final)\n\
    shift\n\
    exec lake exe test-runner-final "$@"\n\
    ;;\n\
    --help|-h)\n\
    echo "lean-cat-nf - Category Normal Form for Lean 4"\n\
    echo ""\n\
    echo "Usage: lean-cat-nf [command] [options]"\n\
    echo ""\n\
    echo "Commands:"\n\
    echo "  bench        Run benchmarks"\n\
    echo "  test         Run test suite"\n\
    echo "  test-final   Run final test runner"\n\
    echo "  --help       Show this help message"\n\
    echo ""\n\
    echo "For more information, visit: https://github.com/fraware/lean-cat-nf"\n\
    ;;\n\
    *)\n\
    echo "Unknown command: $1"\n\
    echo "Use --help for usage information"\n\
    exit 1\n\
    ;;\n\
    esac' > /usr/local/bin/lean-cat-nf && chmod +x /usr/local/bin/lean-cat-nf

# Set default command
ENTRYPOINT ["lean-cat-nf"]
CMD ["--help"]
