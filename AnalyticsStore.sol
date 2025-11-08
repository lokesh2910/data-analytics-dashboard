// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract AnalyticsStore {
    struct AnalysisResult {
        string modelName;
        string rSquared;
        string timestamp;
        string extra;
        address submittedBy;
        uint256 submissionTime;
    }
    
    AnalysisResult[] public results;
    address public owner;
    
    event ResultAdded(uint256 indexed resultId, address indexed submittedBy, string modelName);
    event OwnershipTransferred(address indexed previousOwner, address indexed newOwner);
    
    modifier onlyOwner() {
        require(msg.sender == owner, "Not contract owner");
        _;
    }
    
    constructor() {
        owner = msg.sender;
    }
    
    function addResult(
        string memory _modelName,
        string memory _rSquared,
        string memory _timestamp,
        string memory _extra
    ) external {
        require(bytes(_modelName).length > 0, "Model name cannot be empty");
        require(bytes(_rSquared).length > 0, "R-squared cannot be empty");
        
        results.push(AnalysisResult({
            modelName: _modelName,
            rSquared: _rSquared,
            timestamp: _timestamp,
            extra: _extra,
            submittedBy: msg.sender,
            submissionTime: block.timestamp
        }));
        
        emit ResultAdded(results.length - 1, msg.sender, _modelName);
    }
    
    function getResultsCount() external view returns (uint256) {
        return results.length;
    }
    
    function getResult(uint256 _index) external view returns (
        string memory modelName,
        string memory rSquared,
        string memory timestamp,
        string memory extra,
        address submittedBy
    ) {
        require(_index < results.length, "Index out of bounds");
        AnalysisResult memory result = results[_index];
        return (
            result.modelName,
            result.rSquared,
            result.timestamp,
            result.extra,
            result.submittedBy
        );
    }
    
    function getResultWithDetails(uint256 _index) external view returns (
        string memory modelName,
        string memory rSquared,
        string memory timestamp,
        string memory extra,
        address submittedBy,
        uint256 submissionTime
    ) {
        require(_index < results.length, "Index out of bounds");
        AnalysisResult memory result = results[_index];
        return (
            result.modelName,
            result.rSquared,
            result.timestamp,
            result.extra,
            result.submittedBy,
            result.submissionTime
        );
    }
    
    function transferOwnership(address _newOwner) external onlyOwner {
        require(_newOwner != address(0), "New owner cannot be zero address");
        emit OwnershipTransferred(owner, _newOwner);
        owner = _newOwner;
    }
    
    function withdraw() external onlyOwner {
        payable(owner).transfer(address(this).balance);
    }
    
    receive() external payable {}
}