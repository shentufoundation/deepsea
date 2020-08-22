// node token.js token.bytecode
const Chain = require("@alipay/mychain/index.node") // download from https://tech.antfin.com/docs/2/107128
const fs = require("fs")

const accountKey = fs.readFileSync("", { encoding: "utf8" }) // read from customized .pem password. eg: "./certs/password.pem"
const accountPassword = ""  //need to replace to customized .pem file's password

const keyInfo = Chain.utils.getKeyInfo(accountKey, accountPassword)

const passphrase = "" //need to replace to custom client.key's password
//setting
let opt = {
  host: '47.103.163.48',    //get from your free ant blockChain
  port: 18130,          //port number
  timeout: 30000,
  cert: fs.readFileSync("./certs/client.crt", { encoding: "utf8" }), // client.crt
  ca: fs.readFileSync("./certs/ca.crt", { encoding: "utf8" }),      // ca.crt
  key: fs.readFileSync("./certs/client.key", { encoding: "utf8" }), // client key
  userPublicKey: keyInfo.publicKey,
  userPrivateKey: keyInfo.privateKey,
  userRecoverPublicKey: keyInfo.publicKey,
  userRecoverPrivateKey: keyInfo.privateKey,
  passphrase: passphrase
}

// initialize a connection
const chain = Chain(opt)

/////////////this code is used for create a new account////////////////////////////////
//                                                                      ///////////////
// const newKey = Chain.utils.generateECKey();                          ///////////////
// console.log('newKey priKey:', newKey.privateKey.toString('hex'))     ///////////////
// console.log('newKey pubKey:', newKey.publicKey.toString('hex'))      ///////////////
//                                                                      ///////////////
// const newAccountName = 'Tester001' + Date.now()                      ///////////////
// chain.ctr.CreateAccount({                                            ///////////////
//   from: 'Tester001',                                                 ///////////////
//   to: newAccountName,                                                ///////////////
//   data: {                                                            ///////////////
//     recover_key: '0x'+ newKey.publicKey.toString('hex'),             ///////////////
//     auth_key: '0x'+ newKey.publicKey.toString('hex'),                ///////////////
//     auth_weight: 100                                                 ///////////////
//   }                                                                  ///////////////
// }, (err, data) => {                                                  ///////////////
//   console.log('New Account:', data)                                  ///////////////
//                                                                      ///////////////
//   opt.userPrivateKey = '0x'+ newKey.privateKey.toString('hex')       ///////////////
//   opt.userPublicKey = '0x'+ newKey.publicKey.toString('hex')         ///////////////
//   opt.userRecoverPrivateKey = '0x'+ newKey.privateKey.toString('hex')///////////////
//   opt.userRecoverPublicKey = '0x'+ newKey.publicKey.toString('hex')  ///////////////
//                                                                      ///////////////
//   // chain.setUserKey(opt)                                           ///////////////
//   // chain.setUserRecoverKey(opt)                                    ///////////////
//                                                                      ///////////////
// })                                                                   ///////////////
// chain.ctr.QueryAccount({                                             ///////////////
//   from: 'Tester001'                                                  ///////////////
// }, (err, data) => {                                                  ///////////////
//   console.log('QueryAccount Tester001:', data)                       ///////////////
// })                                                                   ///////////////
// eg:                                                                  ///////////////
// User name : Tester001                                                ///////////////
//                                                                      ///////////////
///////////////////////////////////////////////////////////////////////////////////////

// ./dsc_antchain contracts/token/token.ds abi
const abi = [ {"type":"constructor",
   "name":"constructor",
   "inputs":[],
   "outputs":[],
   "payable":false,
   "constant":false,
   "stateMutability":"nonpayable"},
 {"type":"function",
   "name":"totalSupply",
   "inputs":[],
   "outputs":[{"name":"", "type":"uint256"}],
   "payable":false,
   "constant":true,
   "stateMutability":"view"},
 {"type":"function",
   "name":"balanceOf",
   "inputs":[{"name":"tokenOwner", "type":"identity"}],
   "outputs":[{"name":"", "type":"uint256"}],
   "payable":false,
   "constant":true,
   "stateMutability":"view"},
 {"type":"function",
   "name":"transfer",
   "inputs":[{"name":"toA", "type":"identity"},{"name":"tokens", "type":"uint256"}],
   "outputs":[{"name":"", "type":"bool"}],
   "payable":true,
   "constant":false,
   "stateMutability":"payable"}];

const bytecode = fs.readFileSync(process.argv[2]).toString().replace(/\n|\t|\r| /g, "");

// add date to make sure there is no duplicate hash
const contractName = 'contract'+Date.now()
// initalize a contract example
let myContract = chain.ctr.contract(contractName, abi)

myContract.new(bytecode, {
  from: '', // from your user name eg: Tester001
  parameters: []
}, (err, contract, data) => {
  console.log('contract deploy result:', data)
  myContract.totalSupply({
       from: ''
     }, (err, output, data) => {
       // console.log("contract call error: ", err)
       // console.log('contract call data: :', data)
       console.log('contract call output:', output)
      // replace fake identity by a real identity, example: 0xc60a9d48105950a0cca07a4c6320b98c303ad42d694a634529e8e1a0a16fcdb5
      myContract.balanceOf(('fake identity') , {
          from: ''
        }, (err, output, data) => {
          // console.log("contract call error: ", err)
          // console.log('contract call data: ', data)
          console.log('contract call output:', output)
          //100 is just an example, change to the number you want to send.
          myContract.transfer('fake identity', 100 , {
            from: ''
          }, (err, output, data) => {
            // console.log("contract call error: ", err)
            // console.log('contract call data transfer: ', data)
            console.log('contract call output transfer: ', output)
            myContract.balanceOf(('fake identity') , {
              from: ''
            }, (err, output, data) => {
              // console.log("contract call error: ", err)
              // console.log('contract call data: ', data)
              console.log('contract call output: ', output)
              myContract.balanceOf(('fake identity') , {
                 from: ''
               }, (err, output, data) => {
                 // console.log("contract call error: ", err)
                 // console.log('contract call data: ', data)
                 console.log('contract call output: ', output)
               });
            });
          });
        });
     });
})
