# syntax=docker/dockerfile:1
# Shared Lean toolchain location so the runtime user can invoke `lake exe` without reinstalling elan.
ARG ELAN_ROOT=/opt/elan

FROM ubuntu:22.04 AS builder

ARG ELAN_ROOT
ENV ELAN_HOME="${ELAN_ROOT}"
ENV PATH="${ELAN_ROOT}/bin:${PATH}"

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    git \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

RUN curl -sSf https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh \
    | sh -s -- -y --no-modify-path --default-toolchain none

WORKDIR /app

COPY lean-toolchain Lakefile.lean lake-manifest.json ./
COPY src/ ./src/
COPY scripts/docker-entrypoint.sh /usr/local/bin/lean-cat-nf

RUN elan toolchain install "$(cat lean-toolchain)"
RUN chmod +x /usr/local/bin/lean-cat-nf

ENV LEAN_NUM_THREADS=4
RUN lake exe cache get || true
RUN lake build
RUN lake build bench || true
RUN lake build test-runner || true
RUN lake build test-runner-final || true

RUN useradd --create-home --uid 10001 --shell /bin/bash catnf \
    && chown -R catnf:catnf /app "${ELAN_HOME}"

USER catnf
WORKDIR /app

ENTRYPOINT ["/usr/local/bin/lean-cat-nf"]
CMD ["--help"]
