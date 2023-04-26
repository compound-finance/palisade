// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.15;

interface CErc20Interface {
  function underlying() external view returns (address);
}

interface CTokenInterface {
    function transfer(address dst, uint amount) external returns (bool);
    function transferFrom(address src, address dst, uint amount) external returns (bool);
    function approve(address spender, uint amount) external returns (bool);
    function allowance(address owner, address spender) external view returns (uint);
    function balanceOf(address owner) external view returns (uint);
    function balanceOfUnderlying(address owner) external returns (uint);
    function getAccountSnapshot(address account) external view returns (uint, uint, uint, uint);
    function borrowRatePerBlock() external view returns (uint);
    function supplyRatePerBlock() external view returns (uint);
    function totalBorrowsCurrent() external returns (uint);
    function borrowBalanceCurrent(address account) external returns (uint);
    function borrowBalanceStored(address account) external view returns (uint);
    function exchangeRateCurrent() external returns (uint);
    function exchangeRateStored() external view returns (uint);
    function getCash() external view returns (uint);
    function accrueInterest() external returns (uint);
    function seize(address liquidator, address borrower, uint seizeTokens) external returns (uint);

    function comptroller() external returns (address);

    function symbol() external view returns (string memory);
    function decimals() external view returns (uint8);

    function reserveFactorMantissa() external view returns (uint);
    function totalBorrows() external view returns (uint);
    function totalReserves() external view returns (uint);
    function totalSupply() external view returns (uint);
}

interface PriceOracleInterface {
    function getUnderlyingPrice(CTokenInterface cToken) external view returns (uint);
}

interface EIP20Interface {
    function name() external view returns (string memory);
    function symbol() external view returns (string memory);
    function decimals() external view returns (uint8);
    function balanceOf(address owner) external view returns (uint256 balance);
    function allowance(address owner, address spender) external view returns (uint256 remaining);
}

interface CompInterface {
    function balanceOf(address account) external view returns (uint);
    function getCurrentVotes(address account) external view returns (uint96);
    function delegates(address) external view returns (address);
    function getPriorVotes(address account, uint blockNumber) external view returns (uint96);
}

interface ComptrollerLensInterface {
    function markets(address) external view returns (bool, uint);
    function oracle() external view returns (PriceOracleInterface);
    function getAccountLiquidity(address) external view returns (uint, uint, uint);
    function getAssetsIn(address) external view returns (CTokenInterface[] memory);
    function claimComp(address) external;
    function compAccrued(address) external view returns (uint);
    function compSpeeds(address) external view returns (uint);
    function compSupplySpeeds(address) external view returns (uint);
    function compBorrowSpeeds(address) external view returns (uint);
    function borrowCaps(address) external view returns (uint);
    function mintGuardianPaused(address) external view returns (bool);
    function closeFactorMantissa() external view returns (uint);
    function liquidationIncentiveMantissa() external view returns (uint);
}

contract CompoundLens {
    struct CTokenAllData {
        address cToken;
        uint exchangeRateCurrent;
        uint supplyRatePerBlock;
        uint borrowRatePerBlock;
        uint reserveFactorMantissa;
        uint totalBorrows;
        uint totalReserves;
        uint totalSupply;
        uint totalCash;
        bool isListed;
        uint collateralFactorMantissa;
        address underlyingAssetAddress;
        uint cTokenDecimals;
        uint underlyingDecimals;
        uint compSupplySpeed;
        uint compBorrowSpeed;
        uint borrowCap;
        bool mintGuardianPaused;
        uint underlyingPrice;
    }

    struct NoAccountAllData {
        uint closeFactorMantissa;
        uint liquidationIncentiveMantissa;
        CTokenAllData[] cTokens;
    }

    struct CTokenAllDataWithAccount {
        address cToken;
        uint exchangeRateCurrent;
        uint supplyRatePerBlock;
        uint borrowRatePerBlock;
        uint reserveFactorMantissa;
        uint totalBorrows;
        uint totalReserves;
        uint totalSupply;
        uint totalCash;
        bool isListed;
        uint collateralFactorMantissa;
        address underlyingAssetAddress;
        uint cTokenDecimals;
        uint underlyingDecimals;
        uint compSupplySpeed;
        uint compBorrowSpeed;
        uint borrowCap;
        bool mintGuardianPaused;
        uint underlyingPrice;
        uint balanceOf;
        uint borrowBalanceCurrent;
        uint balanceOfUnderlying;
        uint tokenBalance;
        uint tokenAllowance;
    }

    struct AccountAllData {
        uint closeFactorMantissa;
        uint liquidationIncentiveMantissa;
        CTokenInterface[] marketsIn;
        uint liquidity;
        uint shortfall;
        CompBalanceMetadataExt compMetadata;
        CTokenAllDataWithAccount[] cTokens;
    }

    function getCompSpeeds(ComptrollerLensInterface comptroller, CTokenInterface cToken) internal returns (uint, uint) {
        // Getting comp speeds is gnarly due to not every network having the
        // split comp speeds from Proposal 62 and other networks don't even
        // have comp speeds.
        uint compSupplySpeed = 0;
        (bool compSupplySpeedSuccess, bytes memory compSupplySpeedReturnData) =
            address(comptroller).call(
                abi.encodePacked(
                    comptroller.compSupplySpeeds.selector,
                    abi.encode(address(cToken))
                )
            );
        if (compSupplySpeedSuccess) {
            compSupplySpeed = abi.decode(compSupplySpeedReturnData, (uint));
        }

        uint compBorrowSpeed = 0;
        (bool compBorrowSpeedSuccess, bytes memory compBorrowSpeedReturnData) =
            address(comptroller).call(
                abi.encodePacked(
                    comptroller.compBorrowSpeeds.selector,
                    abi.encode(address(cToken))
                )
            );
        if (compBorrowSpeedSuccess) {
            compBorrowSpeed = abi.decode(compBorrowSpeedReturnData, (uint));
        }

        // If the split comp speeds call doesn't work, try the  oldest non-spit version.
        if (!compSupplySpeedSuccess || !compBorrowSpeedSuccess) {
            (bool compSpeedSuccess, bytes memory compSpeedReturnData) =
            address(comptroller).call(
                abi.encodePacked(
                    comptroller.compSpeeds.selector,
                    abi.encode(address(cToken))
                )
            );
            if (compSpeedSuccess) {
                compSupplySpeed = compBorrowSpeed = abi.decode(compSpeedReturnData, (uint));
            }
        }
        return (compSupplySpeed, compBorrowSpeed);
    }

    function buildCTokenAllData(CTokenInterface cToken) public returns (CTokenAllData memory) {
        uint exchangeRateCurrent = cToken.exchangeRateCurrent();
        ComptrollerLensInterface comptroller = ComptrollerLensInterface(address(cToken.comptroller()));
        (bool isListed, uint collateralFactorMantissa) = comptroller.markets(address(cToken));
        address underlyingAssetAddress;
        uint underlyingDecimals;

        if (compareStrings(cToken.symbol(), "cETH")) {
            underlyingAssetAddress = address(0);
            underlyingDecimals = 18;
        } else {
            CErc20Interface cErc20 = CErc20Interface(address(cToken));
            underlyingAssetAddress = cErc20.underlying();
            underlyingDecimals = EIP20Interface(cErc20.underlying()).decimals();
        }

        (uint compSupplySpeed, uint compBorrowSpeed) = getCompSpeeds(comptroller, cToken);

        uint borrowCap = 0;
        (bool borrowCapSuccess, bytes memory borrowCapReturnData) =
            address(comptroller).call(
                abi.encodePacked(
                    comptroller.borrowCaps.selector,
                    abi.encode(address(cToken))
                )
            );
        if (borrowCapSuccess) {
            borrowCap = abi.decode(borrowCapReturnData, (uint));
        }

        PriceOracleInterface priceOracle = comptroller.oracle();

        return CTokenAllData({
            cToken: address(cToken),
            exchangeRateCurrent: exchangeRateCurrent,
            supplyRatePerBlock: cToken.supplyRatePerBlock(),
            borrowRatePerBlock: cToken.borrowRatePerBlock(),
            reserveFactorMantissa: cToken.reserveFactorMantissa(),
            totalBorrows: cToken.totalBorrows(),
            totalReserves: cToken.totalReserves(),
            totalSupply: cToken.totalSupply(),
            totalCash: cToken.getCash(),
            isListed: isListed,
            collateralFactorMantissa: collateralFactorMantissa,
            underlyingAssetAddress: underlyingAssetAddress,
            cTokenDecimals: cToken.decimals(),
            underlyingDecimals: underlyingDecimals,
            compSupplySpeed: compSupplySpeed,
            compBorrowSpeed: compBorrowSpeed,
            borrowCap: borrowCap,
            mintGuardianPaused: comptroller.mintGuardianPaused(address(cToken)),
            underlyingPrice: priceOracle.getUnderlyingPrice(cToken)
        });
    }

    function queryAllNoAccount(CTokenInterface[] calldata cTokens) external returns (NoAccountAllData memory) {
        uint cTokenCount = cTokens.length;
        CTokenAllData[] memory cTokensRes = new CTokenAllData[](cTokenCount);
        for (uint i = 0; i < cTokenCount; i++) {
            cTokensRes[i] = buildCTokenAllData(cTokens[i]);
        }

        uint liquidationIncentive = 0;
        uint closeFactor = 0;
        if(cTokenCount > 0) {
            ComptrollerLensInterface comptroller = ComptrollerLensInterface(address(cTokens[0].comptroller()));
            liquidationIncentive = comptroller.liquidationIncentiveMantissa();
            closeFactor = comptroller.closeFactorMantissa();
        }

        return NoAccountAllData({
            closeFactorMantissa: closeFactor,
            liquidationIncentiveMantissa: liquidationIncentive,
            cTokens: cTokensRes
        });
    }

    struct CTokenBalances {
        address cToken;
        uint balanceOf;
        uint borrowBalanceCurrent;
        uint balanceOfUnderlying;
        uint tokenBalance;
        uint tokenAllowance;
    }

    function cTokenBalances(CTokenInterface cToken, address payable account) public returns (CTokenBalances memory) {
        uint balanceOf = cToken.balanceOf(account);
        uint borrowBalanceCurrent = cToken.borrowBalanceCurrent(account);
        uint balanceOfUnderlying = cToken.balanceOfUnderlying(account);
        uint tokenBalance;
        uint tokenAllowance;

        if (compareStrings(cToken.symbol(), "cETH")) {
            tokenBalance = account.balance;
            tokenAllowance = account.balance;
        } else {
            CErc20Interface cErc20 = CErc20Interface(address(cToken));
            EIP20Interface underlying = EIP20Interface(cErc20.underlying());
            tokenBalance = underlying.balanceOf(account);
            tokenAllowance = underlying.allowance(account, address(cToken));
        }

        return CTokenBalances({
            cToken: address(cToken),
            balanceOf: balanceOf,
            borrowBalanceCurrent: borrowBalanceCurrent,
            balanceOfUnderlying: balanceOfUnderlying,
            tokenBalance: tokenBalance,
            tokenAllowance: tokenAllowance
        });
    }

    struct AccountLimits {
        CTokenInterface[] markets;
        uint liquidity;
        uint shortfall;
    }

    function getAccountLimits(ComptrollerLensInterface comptroller, address account) public view returns (AccountLimits memory) {
        (uint errorCode, uint liquidity, uint shortfall) = comptroller.getAccountLiquidity(account);
        require(errorCode == 0);

        return AccountLimits({
            markets: comptroller.getAssetsIn(account),
            liquidity: liquidity,
            shortfall: shortfall
        });
    }

    struct CompBalanceMetadataExt {
        uint balance;
        uint votes;
        address delegate;
        uint allocated;
    }

    function getCompBalanceMetadataExt(CompInterface comp, ComptrollerLensInterface comptroller, address account) external returns (CompBalanceMetadataExt memory) {
        uint balance = comp.balanceOf(account);
        comptroller.claimComp(account);
        uint newBalance = comp.balanceOf(account);
        uint accrued = comptroller.compAccrued(account);
        uint total = add(accrued, newBalance, "sum comp total");
        uint allocated = sub(total, balance, "sub allocated");

        return CompBalanceMetadataExt({
            balance: balance,
            votes: uint256(comp.getCurrentVotes(account)),
            delegate: comp.delegates(account),
            allocated: allocated
        });
    }

    function queryAllWithAccount(CTokenInterface[] calldata cTokens, address payable account, CompInterface comp) external returns (AccountAllData memory) {
        uint cTokenCount = cTokens.length;
        CTokenAllDataWithAccount[] memory cTokensRes = new CTokenAllDataWithAccount[](cTokenCount);
        for (uint i = 0; i < cTokenCount; i++) {
            CTokenAllData memory cTokenAllData = buildCTokenAllData(cTokens[i]);
            CTokenBalances memory cTokenBalance = cTokenBalances(cTokens[i], account);
            
            cTokensRes[i] = CTokenAllDataWithAccount({
                cToken: cTokenAllData.cToken,
                exchangeRateCurrent: cTokenAllData.exchangeRateCurrent,
                supplyRatePerBlock: cTokenAllData.supplyRatePerBlock,
                borrowRatePerBlock: cTokenAllData.borrowRatePerBlock,
                reserveFactorMantissa: cTokenAllData.reserveFactorMantissa,
                totalBorrows: cTokenAllData.totalBorrows,
                totalReserves: cTokenAllData.totalReserves,
                totalSupply: cTokenAllData.totalSupply,
                totalCash: cTokenAllData.totalCash,
                isListed: cTokenAllData.isListed,
                collateralFactorMantissa: cTokenAllData.collateralFactorMantissa,
                underlyingAssetAddress: cTokenAllData.underlyingAssetAddress,
                cTokenDecimals: cTokenAllData.cTokenDecimals,
                underlyingDecimals: cTokenAllData.underlyingDecimals,
                compSupplySpeed: cTokenAllData.compSupplySpeed,
                compBorrowSpeed: cTokenAllData.compBorrowSpeed,
                borrowCap: cTokenAllData.borrowCap,
                mintGuardianPaused: cTokenAllData.mintGuardianPaused,
                underlyingPrice: cTokenAllData.underlyingPrice,
                balanceOf: cTokenBalance.balanceOf,
                borrowBalanceCurrent: cTokenBalance.borrowBalanceCurrent,
                balanceOfUnderlying: cTokenBalance.balanceOfUnderlying,
                tokenBalance: cTokenBalance.tokenBalance,
                tokenAllowance: cTokenBalance.tokenAllowance
            });
        }

        //TODO: Put into function?
        uint liquidationIncentive = 0;
        uint closeFactor = 0;

        CTokenInterface[] memory accountMarketsIn;
        uint liquidity = 0;
        uint shortfall = 0;

        uint compBalance = 0;
        uint compVotes = 0;
        address compDelegate;
        uint compAllocated = 0;
        if(cTokenCount > 0) {
            ComptrollerLensInterface comptroller = ComptrollerLensInterface(address(cTokens[0].comptroller()));
            liquidationIncentive = comptroller.liquidationIncentiveMantissa();
            closeFactor = comptroller.closeFactorMantissa();

            AccountLimits memory accountLimits = getAccountLimits(comptroller, account);
            accountMarketsIn = accountLimits.markets;
            liquidity = accountLimits.liquidity;
            shortfall = accountLimits.shortfall;

            CompBalanceMetadataExt memory compMetadata = this.getCompBalanceMetadataExt(comp, comptroller, account);
            compBalance = compMetadata.balance;
            compVotes = compMetadata.votes;
            compDelegate = compMetadata.delegate;
            compAllocated = compMetadata.allocated;
        }

        return AccountAllData({
            closeFactorMantissa: closeFactor,
            liquidationIncentiveMantissa: liquidationIncentive,
            marketsIn: accountMarketsIn,
            liquidity: liquidity,
            shortfall: shortfall,
            compMetadata: CompBalanceMetadataExt({
                balance: compBalance,
                votes: compVotes,
                delegate: compDelegate,
                allocated: compAllocated
            }),
            cTokens: cTokensRes
        });
    }

    struct CTokenUnderlyingPrice {
        address cToken;
        uint underlyingPrice;
    }

    function cTokenUnderlyingPrice(CTokenInterface cToken) public returns (CTokenUnderlyingPrice memory) {
        ComptrollerLensInterface comptroller = ComptrollerLensInterface(address(cToken.comptroller()));
        PriceOracleInterface priceOracle = comptroller.oracle();

        return CTokenUnderlyingPrice({
            cToken: address(cToken),
            underlyingPrice: priceOracle.getUnderlyingPrice(cToken)
        });
    }

    function cTokenUnderlyingPriceAll(CTokenInterface[] calldata cTokens) external returns (CTokenUnderlyingPrice[] memory) {
        uint cTokenCount = cTokens.length;
        CTokenUnderlyingPrice[] memory res = new CTokenUnderlyingPrice[](cTokenCount);
        for (uint i = 0; i < cTokenCount; i++) {
            res[i] = cTokenUnderlyingPrice(cTokens[i]);
        }
        return res;
    }

    function compareStrings(string memory a, string memory b) internal pure returns (bool) {
        return (keccak256(abi.encodePacked((a))) == keccak256(abi.encodePacked((b))));
    }

    function add(uint a, uint b, string memory errorMessage) internal pure returns (uint) {
        uint c = a + b;
        require(c >= a, errorMessage);
        return c;
    }

    function sub(uint a, uint b, string memory errorMessage) internal pure returns (uint) {
        require(b <= a, errorMessage);
        uint c = a - b;
        return c;
    }
}