# Deployment Guide for lean-cat-nf

This document outlines all the deployment artifacts and installation methods available for lean-cat-nf.

## 🚀 Installation Methods

### 1. Docker (Production Ready)

**One-command run:**
```bash
docker run --rm ghcr.io/fraware/lean-cat-nf:latest --help
docker run --rm ghcr.io/fraware/lean-cat-nf:latest bench
docker run --rm ghcr.io/fraware/lean-cat-nf:latest test
```

### 2. One-Command Installation Scripts

**Linux/macOS:**
```bash
curl -sSL https://raw.githubusercontent.com/fraware/lean-cat-nf/main/setup.sh | bash
```

**Windows:**
```powershell
iwr -useb https://raw.githubusercontent.com/fraware/lean-cat-nf/main/setup.bat | iex
```

### 3. Makefile-based Development

```bash
git clone https://github.com/fraware/lean-cat-nf.git
cd lean-cat-nf
make dev    # Set up environment
make test   # Run tests
make bench  # Run benchmarks
make release # Build for release
```

## 📦 Deployment Artifacts

### Docker Images
- **Registry:** GitHub Container Registry (ghcr.io)
- **Images:** 
  - `ghcr.io/fraware/lean-cat-nf:latest`
  - `ghcr.io/fraware/lean-cat-nf:v<version>`
- **Platforms:** linux/amd64, linux/arm64

### Installation Scripts
- **setup.sh** - Unix/Linux installation script
- **setup.bat** - Windows installation script
- Both scripts handle dependency installation and environment setup

### Build System
- **Makefile** - Comprehensive build and deployment targets
- **GitHub Actions** - Automated CI/CD pipeline
- **Docker** - Multi-stage containerized builds

## 🔧 Available Commands

### Makefile Targets
```bash
make help           # Show help
make dev            # Set up development environment
make build          # Build the project
make test           # Run test suite
make bench          # Run benchmarks
make clean          # Clean build artifacts
make install        # Install globally
make docker-build   # Build Docker image
make docker-test    # Test Docker image
make release        # Full release build
make info           # Show project information
```

### Docker Commands
```bash
# Help
docker run --rm ghcr.io/fraware/lean-cat-nf:latest --help

# Benchmarks
docker run --rm ghcr.io/fraware/lean-cat-nf:latest bench

# Tests
docker run --rm ghcr.io/fraware/lean-cat-nf:latest test

# Final tests
docker run --rm ghcr.io/fraware/lean-cat-nf:latest test-final
```

### Installed Binary
```bash
# After installation via setup script or make install
lean-cat-nf --help
lean-cat-nf bench
lean-cat-nf test
```

## 🧪 Testing

### Automated Testing
- **GitHub Actions:** Tests all installation methods on push/PR
- **Multi-platform:** Tests on Ubuntu, macOS, and Windows
- **Security scans:** Trivy vulnerability scanning
- **Integration tests:** Full build and execution tests

### Manual Testing Checklist
- [ ] Docker image builds successfully
- [ ] Docker commands execute without errors
- [ ] Setup scripts complete successfully on target platforms
- [ ] Makefile targets work correctly
- [ ] Installation methods produce working binaries
- [ ] All help commands display correctly
- [ ] Benchmark and test commands execute

## 📋 Pre-release Checklist

Before shipping a release:

1. **Build Tests**
   ```bash
   make release-dry-run
   ```

2. **Docker Tests**
   ```bash
   make docker-build
   make docker-test
   ```

3. **Installation Tests**
   - Test setup scripts on clean systems
   - Verify all installation methods work
   - Check that all commands execute correctly

4. **Documentation**
   - Ensure README.md is up to date
   - Verify all installation commands are correct
   - Check that help outputs are accurate

5. **Security**
   - Run security scans
   - Verify no secrets in scripts
   - Check file permissions

## 🔄 Release Process

1. **Prepare Release**
   ```bash
   make release-check
   make release
   ```

2. **Tag and Push**
   ```bash
   git tag v<version>
   git push --tags
   ```

3. **Publish Docker Images**
   ```bash
   make docker-push
   ```

4. **Verify Deployment**
   - Test all installation methods with new version
   - Verify Docker images are available
   - Check GitHub releases

## 🛠️ Troubleshooting

### Common Issues

**Docker build fails:**
- Check Docker daemon is running
- Ensure sufficient disk space
- Verify internet connectivity for dependencies

**Setup script fails:**
- Check system has curl and git installed
- Verify internet connectivity
- Check file permissions

**Makefile errors:**
- Ensure make is installed
- Check shell compatibility (bash/sh)
- Verify all dependencies are available

**Binary not found after installation:**
- Check PATH includes installation directory
- Restart shell session
- Verify installation completed successfully

### Debug Commands

```bash
# Check project status
make info

# Check dependencies
make check-deps

# Verbose build
make build V=1

# Docker debug
docker run --rm -it ghcr.io/fraware/lean-cat-nf:latest /bin/bash
```

## 📊 Metrics

The deployment setup provides:
- **< 10 minute** setup time for new users
- **One-command** installation options
- **Multi-platform** support (Linux, macOS, Windows)
- **Production-ready** Docker containers
- **Automated** CI/CD pipeline
- **Comprehensive** testing coverage
