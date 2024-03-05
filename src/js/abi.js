const SleuthABI = [
  {
    inputs: [
      {
        internalType: 'contract CTokenInterface',
        name: 'cToken',
        type: 'address',
      },
    ],
    name: 'buildCTokenAllData',
    outputs: [
      {
        components: [
          {
            internalType: 'address',
            name: 'cToken',
            type: 'address',
          },
          {
            internalType: 'uint256',
            name: 'exchangeRateCurrent',
            type: 'uint256',
          },
          {
            internalType: 'uint256',
            name: 'supplyRatePerBlock',
            type: 'uint256',
          },
          {
            internalType: 'uint256',
            name: 'borrowRatePerBlock',
            type: 'uint256',
          },
          {
            internalType: 'uint256',
            name: 'reserveFactorMantissa',
            type: 'uint256',
          },
          {
            internalType: 'uint256',
            name: 'totalBorrows',
            type: 'uint256',
          },
          {
            internalType: 'uint256',
            name: 'totalReserves',
            type: 'uint256',
          },
          {
            internalType: 'uint256',
            name: 'totalSupply',
            type: 'uint256',
          },
          {
            internalType: 'uint256',
            name: 'totalCash',
            type: 'uint256',
          },
          {
            internalType: 'bool',
            name: 'isListed',
            type: 'bool',
          },
          {
            internalType: 'uint256',
            name: 'collateralFactorMantissa',
            type: 'uint256',
          },
          {
            internalType: 'address',
            name: 'underlyingAssetAddress',
            type: 'address',
          },
          {
            internalType: 'uint256',
            name: 'cTokenDecimals',
            type: 'uint256',
          },
          {
            internalType: 'uint256',
            name: 'underlyingDecimals',
            type: 'uint256',
          },
          {
            internalType: 'uint256',
            name: 'compSupplySpeed',
            type: 'uint256',
          },
          {
            internalType: 'uint256',
            name: 'compBorrowSpeed',
            type: 'uint256',
          },
          {
            internalType: 'uint256',
            name: 'borrowCap',
            type: 'uint256',
          },
          {
            internalType: 'bool',
            name: 'mintGuardianPaused',
            type: 'bool',
          },
          {
            internalType: 'uint256',
            name: 'underlyingPrice',
            type: 'uint256',
          },
        ],
        internalType: 'struct SleuthLens.CTokenAllData',
        name: '',
        type: 'tuple',
      },
    ],
    stateMutability: 'nonpayable',
    type: 'function',
  },
  {
    inputs: [
      {
        internalType: 'contract CTokenInterface',
        name: 'cToken',
        type: 'address',
      },
      {
        internalType: 'address payable',
        name: 'account',
        type: 'address',
      },
    ],
    name: 'cTokenBalances',
    outputs: [
      {
        components: [
          {
            internalType: 'address',
            name: 'cToken',
            type: 'address',
          },
          {
            internalType: 'uint256',
            name: 'balanceOf',
            type: 'uint256',
          },
          {
            internalType: 'uint256',
            name: 'borrowBalanceCurrent',
            type: 'uint256',
          },
          {
            internalType: 'uint256',
            name: 'balanceOfUnderlying',
            type: 'uint256',
          },
          {
            internalType: 'uint256',
            name: 'tokenBalance',
            type: 'uint256',
          },
          {
            internalType: 'uint256',
            name: 'tokenAllowance',
            type: 'uint256',
          },
        ],
        internalType: 'struct SleuthLens.CTokenBalances',
        name: '',
        type: 'tuple',
      },
    ],
    stateMutability: 'nonpayable',
    type: 'function',
  },
  {
    inputs: [
      {
        internalType: 'contract ComptrollerLensInterface',
        name: 'comptroller',
        type: 'address',
      },
      {
        internalType: 'address',
        name: 'account',
        type: 'address',
      },
    ],
    name: 'getAccountLimits',
    outputs: [
      {
        components: [
          {
            internalType: 'contract CTokenInterface[]',
            name: 'markets',
            type: 'address[]',
          },
          {
            internalType: 'uint256',
            name: 'liquidity',
            type: 'uint256',
          },
          {
            internalType: 'uint256',
            name: 'shortfall',
            type: 'uint256',
          },
        ],
        internalType: 'struct SleuthLens.AccountLimits',
        name: '',
        type: 'tuple',
      },
    ],
    stateMutability: 'view',
    type: 'function',
  },
  {
    inputs: [
      {
        internalType: 'contract CompInterface',
        name: 'comp',
        type: 'address',
      },
      {
        internalType: 'contract ComptrollerLensInterface',
        name: 'comptroller',
        type: 'address',
      },
      {
        internalType: 'address',
        name: 'account',
        type: 'address',
      },
    ],
    name: 'getCompBalanceMetadataExt',
    outputs: [
      {
        components: [
          {
            internalType: 'uint256',
            name: 'balance',
            type: 'uint256',
          },
          {
            internalType: 'uint256',
            name: 'votes',
            type: 'uint256',
          },
          {
            internalType: 'address',
            name: 'delegate',
            type: 'address',
          },
          {
            internalType: 'uint256',
            name: 'allocated',
            type: 'uint256',
          },
        ],
        internalType: 'struct SleuthLens.CompBalanceMetadataExt',
        name: '',
        type: 'tuple',
      },
    ],
    stateMutability: 'nonpayable',
    type: 'function',
  },
  {
    inputs: [
      {
        internalType: 'contract CTokenInterface[]',
        name: 'cTokens',
        type: 'address[]',
      },
    ],
    name: 'queryAllNoAccount',
    outputs: [
      {
        components: [
          {
            internalType: 'uint256',
            name: 'closeFactorMantissa',
            type: 'uint256',
          },
          {
            internalType: 'uint256',
            name: 'liquidationIncentiveMantissa',
            type: 'uint256',
          },
          {
            components: [
              {
                internalType: 'address',
                name: 'cToken',
                type: 'address',
              },
              {
                internalType: 'uint256',
                name: 'exchangeRateCurrent',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'supplyRatePerBlock',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'borrowRatePerBlock',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'reserveFactorMantissa',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'totalBorrows',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'totalReserves',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'totalSupply',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'totalCash',
                type: 'uint256',
              },
              {
                internalType: 'bool',
                name: 'isListed',
                type: 'bool',
              },
              {
                internalType: 'uint256',
                name: 'collateralFactorMantissa',
                type: 'uint256',
              },
              {
                internalType: 'address',
                name: 'underlyingAssetAddress',
                type: 'address',
              },
              {
                internalType: 'uint256',
                name: 'cTokenDecimals',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'underlyingDecimals',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'compSupplySpeed',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'compBorrowSpeed',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'borrowCap',
                type: 'uint256',
              },
              {
                internalType: 'bool',
                name: 'mintGuardianPaused',
                type: 'bool',
              },
              {
                internalType: 'uint256',
                name: 'underlyingPrice',
                type: 'uint256',
              },
            ],
            internalType: 'struct SleuthLens.CTokenAllData[]',
            name: 'cTokens',
            type: 'tuple[]',
          },
        ],
        internalType: 'struct SleuthLens.NoAccountAllData',
        name: '',
        type: 'tuple',
      },
    ],
    stateMutability: 'nonpayable',
    type: 'function',
  },
  {
    inputs: [
      {
        internalType: 'contract CTokenInterface[]',
        name: 'cTokens',
        type: 'address[]',
      },
      {
        internalType: 'address payable',
        name: 'account',
        type: 'address',
      },
      {
        internalType: 'contract CompInterface',
        name: 'comp',
        type: 'address',
      },
      {
        internalType: 'address',
        name: 'capFactory',
        type: 'address',
      },
    ],
    name: 'queryAllWithAccount',
    outputs: [
      {
        components: [
          {
            internalType: 'uint256',
            name: 'closeFactorMantissa',
            type: 'uint256',
          },
          {
            internalType: 'uint256',
            name: 'liquidationIncentiveMantissa',
            type: 'uint256',
          },
          {
            internalType: 'contract CTokenInterface[]',
            name: 'marketsIn',
            type: 'address[]',
          },
          {
            internalType: 'uint256',
            name: 'liquidity',
            type: 'uint256',
          },
          {
            internalType: 'uint256',
            name: 'shortfall',
            type: 'uint256',
          },
          {
            components: [
              {
                internalType: 'uint256',
                name: 'balance',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'votes',
                type: 'uint256',
              },
              {
                internalType: 'address',
                name: 'delegate',
                type: 'address',
              },
              {
                internalType: 'uint256',
                name: 'allocated',
                type: 'uint256',
              },
            ],
            internalType: 'struct SleuthLens.CompBalanceMetadataExt',
            name: 'compMetadata',
            type: 'tuple',
          },
          {
            internalType: 'uint256',
            name: 'capFactoryAllowance',
            type: 'uint256',
          },
          {
            components: [
              {
                internalType: 'address',
                name: 'cToken',
                type: 'address',
              },
              {
                internalType: 'uint256',
                name: 'exchangeRateCurrent',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'supplyRatePerBlock',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'borrowRatePerBlock',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'reserveFactorMantissa',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'totalBorrows',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'totalReserves',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'totalSupply',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'totalCash',
                type: 'uint256',
              },
              {
                internalType: 'bool',
                name: 'isListed',
                type: 'bool',
              },
              {
                internalType: 'uint256',
                name: 'collateralFactorMantissa',
                type: 'uint256',
              },
              {
                internalType: 'address',
                name: 'underlyingAssetAddress',
                type: 'address',
              },
              {
                internalType: 'uint256',
                name: 'cTokenDecimals',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'underlyingDecimals',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'compSupplySpeed',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'compBorrowSpeed',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'borrowCap',
                type: 'uint256',
              },
              {
                internalType: 'bool',
                name: 'mintGuardianPaused',
                type: 'bool',
              },
              {
                internalType: 'uint256',
                name: 'underlyingPrice',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'balanceOf',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'borrowBalanceCurrent',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'balanceOfUnderlying',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'tokenBalance',
                type: 'uint256',
              },
              {
                internalType: 'uint256',
                name: 'tokenAllowance',
                type: 'uint256',
              },
            ],
            internalType: 'struct SleuthLens.CTokenAllDataWithAccount[]',
            name: 'cTokens',
            type: 'tuple[]',
          },
        ],
        internalType: 'struct SleuthLens.AccountAllData',
        name: '',
        type: 'tuple',
      },
    ],
    stateMutability: 'nonpayable',
    type: 'function',
  },
];

export { SleuthABI };
