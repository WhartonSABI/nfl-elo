#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
RUN_ALL_SCRIPT="${PROJECT_ROOT}/scripts/run-all.R"
HUDL_DIR="${PROJECT_ROOT}/data/hudl"

CHECK_ONLY=0

log() {
  printf '[run-full-pipeline] %s\n' "$1"
}

fail() {
  printf '[run-full-pipeline] ERROR: %s\n' "$1" >&2
  exit 1
}

usage() {
  cat <<'EOF'
Usage: ./scripts/run-full-pipeline.sh [--check-only]

Options:
  --check-only   Run preflight checks and print resolved settings without
                 running scripts/run-all.R.
  -h, --help     Show this help text.

Environment overrides (all optional):
  FORCE_REBUILD_INPUTS      default: 0
  FORCE_REBUILD_MODELING    default: 0
  END_TO_END_BOOTSTRAP_ITER default: 1000
  PATH_BOOTSTRAP_ITER       default: 100
  PIPELINE_WORKERS          default: auto-detected (max(1, cores - 4))
EOF
}

while (($# > 0)); do
  case "$1" in
    --check-only)
      CHECK_ONLY=1
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      fail "Unknown argument: $1 (run with --help for usage)"
      ;;
  esac
  shift
done

require_command() {
  local cmd="$1"
  command -v "${cmd}" >/dev/null 2>&1 || fail "Missing required command: ${cmd}"
}

file_exists() {
  local path="$1"
  [[ -f "${path}" ]]
}

detect_cores() {
  local raw=""

  if command -v sysctl >/dev/null 2>&1; then
    raw="$(sysctl -n hw.logicalcpu 2>/dev/null || true)"
  fi

  if [[ -z "${raw}" ]] && command -v nproc >/dev/null 2>&1; then
    raw="$(nproc 2>/dev/null || true)"
  fi

  if [[ -z "${raw}" ]] && command -v getconf >/dev/null 2>&1; then
    raw="$(getconf _NPROCESSORS_ONLN 2>/dev/null || true)"
  fi

  if [[ "${raw}" =~ ^[0-9]+$ ]] && ((raw > 0)); then
    printf '%s\n' "${raw}"
  else
    printf '1\n'
  fi
}

default_workers() {
  local cores workers
  cores="$(detect_cores)"
  workers=$((cores - 4))
  if ((workers < 1)); then
    workers=1
  fi
  printf '%s\n' "${workers}"
}

ensure_required_inputs() {
  local freeze_frames_a freeze_frames_b roster
  freeze_frames_a="${HUDL_DIR}/Hudl IQ 2021 NFL freeze frames.csv"
  freeze_frames_b="${HUDL_DIR}/Hudl IQ 2021 NFL Events + Freeze Frame.csv"
  roster="${HUDL_DIR}/Hudl IQ 2021 player roster.csv"

  [[ -d "${HUDL_DIR}" ]] || fail "Missing directory: ${HUDL_DIR}"
  if ! file_exists "${freeze_frames_a}" && ! file_exists "${freeze_frames_b}"; then
    fail "Missing freeze frame file in ${HUDL_DIR}. Need either:
  - Hudl IQ 2021 NFL freeze frames.csv
  - Hudl IQ 2021 NFL Events + Freeze Frame.csv"
  fi
  file_exists "${roster}" || fail "Missing required file: ${roster}"
}

require_command Rscript
file_exists "${RUN_ALL_SCRIPT}" || fail "Missing pipeline runner: ${RUN_ALL_SCRIPT}"
ensure_required_inputs

if [[ -z "${FORCE_REBUILD_INPUTS:-}" ]]; then
  export FORCE_REBUILD_INPUTS=0
fi
if [[ -z "${FORCE_REBUILD_MODELING:-}" ]]; then
  export FORCE_REBUILD_MODELING=0
fi
if [[ -z "${END_TO_END_BOOTSTRAP_ITER:-}" ]]; then
  export END_TO_END_BOOTSTRAP_ITER=1000
fi
if [[ -z "${PATH_BOOTSTRAP_ITER:-}" ]]; then
  export PATH_BOOTSTRAP_ITER=100
fi
if [[ -z "${PIPELINE_WORKERS:-}" ]]; then
  export PIPELINE_WORKERS="$(default_workers)"
fi

log "Preflight checks passed."
log "PROJECT_ROOT=${PROJECT_ROOT}"
log "FORCE_REBUILD_INPUTS=${FORCE_REBUILD_INPUTS}"
log "FORCE_REBUILD_MODELING=${FORCE_REBUILD_MODELING}"
log "END_TO_END_BOOTSTRAP_ITER=${END_TO_END_BOOTSTRAP_ITER}"
log "PATH_BOOTSTRAP_ITER=${PATH_BOOTSTRAP_ITER}"
log "PIPELINE_WORKERS=${PIPELINE_WORKERS}"

if ((CHECK_ONLY == 1)); then
  log "Check-only mode requested; exiting without running pipeline."
  exit 0
fi

cd "${PROJECT_ROOT}"
log "Running scripts/run-all.R ..."
Rscript "${RUN_ALL_SCRIPT}"
log "Pipeline run complete."
