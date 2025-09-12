# CatNF Comprehensive Testing Framework

This directory contains a comprehensive testing framework for the CatNF (Categorical Normal Form) system, designed for production use with state-of-the-art software engineering practices.

## Overview

The testing framework provides four main categories of tests:

1. **Unit Tests** - Test individual modules in isolation
2. **Integration Tests** - Test complete workflows and end-to-end scenarios
3. **Performance Tests** - Benchmark performance and detect regressions
4. **Determinism Tests** - Ensure deterministic behavior and detect non-determinism

## Directory Structure

```
tests/
├── Unit/                    # Unit tests for individual modules
│   ├── Core.lean           # Core module unit tests
│   ├── Attr.lean           # Attribute system unit tests
│   ├── Monoidal.lean       # Monoidal category unit tests
│   ├── RewriteRules.lean   # Rewrite rules unit tests
│   └── Tactic.lean         # Tactic system unit tests
├── Integration/             # Integration tests
│   ├── Workflows.lean      # Complete workflow tests
│   └── EndToEnd.lean       # End-to-end integration tests
├── Performance/             # Performance testing
│   ├── Benchmarks.lean     # Performance benchmarks
│   └── RegressionDetection.lean # Regression detection
├── Determinism/             # Determinism testing
│   ├── DeterminismTests.lean # Determinism verification
│   └── NonDeterminismDetection.lean # Non-determinism detection
├── TestRunner.lean         # Main test runner
└── README.md              # This file
```

## Test Categories

### Unit Tests

Unit tests verify the correctness of individual modules in isolation. Each module has comprehensive test coverage including:

- **Core Module Tests** (`Unit/Core.lean`)
  - Expression parsing and validation
  - Composition flattening
  - Identity handling
  - Isomorphism transport
  - Segment processing
  - Expression rebuilding

- **Attribute System Tests** (`Unit/Attr.lean`)
  - Rule registration and validation
  - Rule application
  - Rule database management
  - Error handling
  - Concurrency safety

- **Monoidal Category Tests** (`Unit/Monoidal.lean`)
  - Tensor operations
  - Associativity rules
  - Unitor handling
  - Braiding and symmetry
  - Coherence normalization

- **Rewrite Rules Tests** (`Unit/RewriteRules.lean`)
  - Rule application
  - Normalization
  - Confluence checking
  - Termination verification

- **Tactic System Tests** (`Unit/Tactic.lean`)
  - Tactic implementation
  - Configuration handling
  - Error handling
  - Performance optimization

### Integration Tests

Integration tests verify that different modules work together correctly:

- **Workflow Tests** (`Integration/Workflows.lean`)
  - Complete normalization workflows
  - Monoidal category workflows
  - Functor and whiskering workflows
  - Isomorphism transport workflows
  - Complex mixed expression workflows
  - Error handling workflows
  - Performance workflows
  - Determinism workflows

- **End-to-End Tests** (`Integration/EndToEnd.lean`)
  - Complete categorical normalization
  - Monoidal category with all operations
  - Functor and whiskering with all operations
  - Isomorphism transport with all operations
  - Mixed operations with all features
  - Error handling and edge cases
  - Performance with large expressions
  - Determinism with repeated operations

### Performance Tests

Performance tests ensure the system meets performance requirements and detect regressions:

- **Benchmarks** (`Performance/Benchmarks.lean`)
  - Simple composition normalization
  - Nested composition normalization
  - Monoidal tensor normalization
  - Functor map normalization
  - Whiskering normalization
  - Isomorphism transport normalization
  - Complex mixed expression normalization

- **Regression Detection** (`Performance/RegressionDetection.lean`)
  - Statistical analysis of performance metrics
  - Regression threshold detection
  - Performance baseline management
  - Confidence interval analysis
  - Outlier detection
  - Normal distribution verification

### Determinism Tests

Determinism tests ensure the system produces consistent results:

- **Determinism Verification** (`Determinism/DeterminismTests.lean`)
  - Result consistency checking
  - Execution time variance analysis
  - Memory usage variance analysis
  - Side effect consistency
  - Statistical significance testing

- **Non-Determinism Detection** (`Determinism/NonDeterminismDetection.lean`)
  - High variance detection
  - Inconsistent result detection
  - Statistical significance analysis
  - Outlier detection
  - Normal distribution verification

## Running Tests

### Quick Tests (Development)

For quick testing during development:

```lean
# Run quick tests (unit + integration only)
CatNF.Tests.TestRunner.runQuickTests
```

### Full Tests (CI/CD)

For comprehensive testing in CI/CD pipelines:

```lean
# Run full test suite
CatNF.Tests.TestRunner.runFullTests
```

### Custom Configuration

For custom test configurations:

```lean
let config : TestConfig := {
  enableUnitTests := true
  enableIntegrationTests := true
  enablePerformanceTests := true
  enableDeterminismTests := true
  enableRegressionDetection := true
  enableNonDeterminismDetection := true
  timeoutMs := 300000
  memoryLimitMB := 2000
  enableDetailedLogging := true
  enablePerformanceLogging := true
  enableStatisticalAnalysis := true
  maxConcurrentTests := 4
  retryFailedTests := true
  maxRetries := 3
}

let result ← CatNF.Tests.TestRunner.runAllTests config
```

## Test Configuration

The `TestConfig` structure allows fine-grained control over test execution:

- `enableUnitTests`: Enable/disable unit tests
- `enableIntegrationTests`: Enable/disable integration tests
- `enablePerformanceTests`: Enable/disable performance tests
- `enableDeterminismTests`: Enable/disable determinism tests
- `enableRegressionDetection`: Enable/disable regression detection
- `enableNonDeterminismDetection`: Enable/disable non-determinism detection
- `timeoutMs`: Maximum execution time per test
- `memoryLimitMB`: Maximum memory usage per test
- `enableDetailedLogging`: Enable detailed test logging
- `enablePerformanceLogging`: Enable performance metrics logging
- `enableStatisticalAnalysis`: Enable statistical analysis
- `maxConcurrentTests`: Maximum concurrent test execution
- `retryFailedTests`: Retry failed tests
- `maxRetries`: Maximum number of retries

## Test Results

The testing framework provides comprehensive result reporting:

### TestExecutionResult
- `testSuite`: Name of the test suite
- `testName`: Name of the specific test
- `passed`: Whether the test passed
- `executionTimeMs`: Execution time in milliseconds
- `memoryUsageBytes`: Memory usage in bytes
- `errorMessage`: Error message if test failed
- `details`: Additional test details

### TestSuiteResult
- `suiteName`: Name of the test suite
- `totalTests`: Total number of tests in the suite
- `passedTests`: Number of tests that passed
- `failedTests`: Number of tests that failed
- `executionTimeMs`: Total execution time for the suite
- `memoryUsageBytes`: Total memory usage for the suite
- `results`: List of individual test results

### OverallTestResult
- `totalSuites`: Total number of test suites
- `totalTests`: Total number of tests across all suites
- `passedSuites`: Number of test suites that passed
- `failedSuites`: Number of test suites that failed
- `passedTests`: Total number of tests that passed
- `failedTests`: Total number of tests that failed
- `totalExecutionTimeMs`: Total execution time across all suites
- `totalMemoryUsageBytes`: Total memory usage across all suites
- `suiteResults`: List of test suite results

## Performance Metrics

The framework tracks comprehensive performance metrics:

- **Execution Time**: Wall-clock time for test execution
- **Memory Usage**: Memory consumption during test execution
- **Steps Executed**: Number of normalization steps performed
- **Expressions Processed**: Number of expressions processed
- **Average Time Per Expression**: Average processing time per expression
- **Peak Memory Usage**: Maximum memory usage during execution
- **Cache Hits/Misses**: Cache performance metrics
- **Statistical Analysis**: Mean, variance, standard deviation, confidence intervals

## Determinism Verification

The framework ensures deterministic behavior through:

- **Result Consistency**: Verifies that identical inputs produce identical outputs
- **Execution Time Variance**: Monitors execution time consistency
- **Memory Usage Variance**: Monitors memory usage consistency
- **Side Effect Consistency**: Ensures no unexpected side effects
- **Statistical Analysis**: Uses statistical methods to detect non-determinism
- **Outlier Detection**: Identifies unusual behavior patterns

## Regression Detection

The framework detects performance regressions through:

- **Baseline Comparison**: Compares current performance to established baselines
- **Threshold Detection**: Identifies performance degradation beyond acceptable thresholds
- **Statistical Significance**: Uses statistical methods to determine significance
- **Confidence Intervals**: Provides confidence intervals for performance metrics
- **Trend Analysis**: Identifies performance trends over time

## Best Practices

The testing framework follows industry best practices:

1. **Deterministic Testing**: All tests produce consistent results
2. **Comprehensive Coverage**: Tests cover all modules and workflows
3. **Performance Monitoring**: Continuous performance monitoring and regression detection
4. **Statistical Analysis**: Uses statistical methods for reliable results
5. **Error Handling**: Comprehensive error handling and reporting
6. **Documentation**: Thorough documentation of all test cases
7. **Maintainability**: Well-structured, maintainable test code
8. **Scalability**: Designed to scale with the system

## Continuous Integration

The testing framework is designed for integration with CI/CD pipelines:

- **Automated Execution**: Tests run automatically on code changes
- **Performance Monitoring**: Continuous performance monitoring
- **Regression Detection**: Automatic detection of performance regressions
- **Determinism Verification**: Continuous verification of deterministic behavior
- **Comprehensive Reporting**: Detailed test results and metrics
- **Failure Analysis**: Detailed analysis of test failures

## Maintenance

The testing framework requires regular maintenance:

1. **Update Baselines**: Update performance baselines as the system evolves
2. **Review Thresholds**: Review and adjust regression detection thresholds
3. **Add Test Cases**: Add new test cases for new features
4. **Update Documentation**: Keep documentation up to date
5. **Monitor Performance**: Continuously monitor test execution performance
6. **Analyze Results**: Regularly analyze test results for insights

## Conclusion

This comprehensive testing framework provides production-ready testing capabilities for the CatNF system, ensuring reliability, performance, and deterministic behavior. The framework follows state-of-the-art software engineering practices and is designed for continuous integration and long-term maintenance.
