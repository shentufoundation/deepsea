module.exports = {
    to256CallData: data => {
        return BigInt(data).toString(16).padStart(64, '0');
    },
    toHex: data => {
        return Array.from(data).map((value) => value.toString(16).padStart(2, '0')).join('');
    },
    toLEHex: data => {
        return Array.from(data).map((value) => value.toString(16).padStart(2, '0')).reverse().join('');
    },
    toBigInt: hexRepr => {
        return BigInt(hexRepr);
    },
}
