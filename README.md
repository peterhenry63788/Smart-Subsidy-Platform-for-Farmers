# 🌾 Smart Subsidy Platform for Farmers

A blockchain-based platform for transparent and milestone-driven agricultural subsidy distribution using Clarity smart contracts on the Stacks blockchain.

## 📋 Overview

This smart contract enables governments and NGOs to distribute subsidies to verified farmers through a transparent, milestone-based system with oracle verification for crop outputs and farming progress.

## ✨ Features

- 👨‍🌾 **Farmer Registration & Verification**: Secure farmer onboarding with verification system
- 💰 **Milestone-Based Disbursements**: Subsidies released in stages based on farming progress
- 🔍 **Oracle Integration**: Third-party verification of farming milestones and crop yields
- 📊 **Transparent Tracking**: Real-time monitoring of subsidy distribution and progress
- 🛡️ **Secure Fund Management**: Protected contract funds with owner-only controls

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

1. Clone the repository
2. Navigate to the project directory
3. Initialize Clarinet project:

```bash
clarinet console
```

## 📖 Usage Guide

### For Contract Owners (Government/NGO)

#### 1. 💰 Fund the Contract
```clarity
(contract-call? .Smart-Subsidy-Platform-for-Farmers fund-contract)
```

#### 2. ✅ Verify Farmers
```clarity
(contract-call? .Smart-Subsidy-Platform-for-Farmers verify-farmer u1)
```

#### 3. 🎯 Create Subsidy Programs
```clarity
(contract-call? .Smart-Subsidy-Platform-for-Farmers create-subsidy u1 u10000 "wheat" u5000)
```

#### 4. 👥 Authorize Oracles
```clarity
(contract-call? .Smart-Subsidy-Platform-for-Farmers authorize-oracle 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

#### 5. 💸 Disburse Milestone Payments
```clarity
(contract-call? .Smart-Subsidy-Platform-for-Farmers disburse-milestone u1 u1)
```

### For Farmers

#### 1. 📝 Register as Farmer
```clarity
(contract-call? .Smart-Subsidy-Platform-for-Farmers register-farmer "John Doe" "Iowa, USA" u100)
```

#### 2. 🌾 Report Harvest Results
```clarity
(contract-call? .Smart-Subsidy-Platform-for-Farmers report-harvest u1 u4800)
```

### For Oracles

#### 1. ✅ Verify Milestones
