#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
lean-cat-nf - Category Normal Form for Lean 4

Usage: lean-cat-nf [command] [args...]

Commands:
  bench         Run lake exe bench
  test          Run lake exe test-runner
  test-final    Run lake exe test-runner-final
  --help, -h    Show this message

Examples:
  docker run --rm IMAGE bench
  docker run --rm IMAGE test
EOF
}

cd /app

case "${1:-}" in
  ""|--help|-h)
    usage
    ;;
  bench)
    shift
    exec lake exe bench "$@"
    ;;
  test)
    shift
    exec lake exe test-runner "$@"
    ;;
  test-final)
    shift
    exec lake exe test-runner-final "$@"
    ;;
  *)
    echo "Unknown command: $1" >&2
    usage >&2
    exit 1
    ;;
esac
