import MistralClient from '@mistralai/mistralai';
import * as fs from 'fs';
import * as path from 'path';
import * as dotenv from 'dotenv';
import express from "express";
dotenv.config();

async function typecheck(program, spec, client) {
  let message = "The spec of the programming language is:\n" + spec + " The program is as follows:\n " + program + " \n Is the program correct given the spec? Give a one word yes or no answer." 

  const chatResponse = await client.chat({
    model: 'mistral-tiny',
    messages: [{role: 'user', content: message}],
  });
  
  console.log(chatResponse.choices[0].message.content);

  let textRepsonse = chatResponse.choices[0].message.content;
  textRepsonse = textRepsonse.split(' ');
  const result = textRepsonse[0].substring(0, textRepsonse[0].length - 1);

  console.log('Result: ', result);

  if (result.toLowerCase() === 'yes') {
    return true;
  } else {
    return false;
  }
}

async function doesHalt(program, spec, client) {
  let message = "The spec for the programming language is:\n" + spec + "\nDoes the following program halt given the spec?:\n " + program + "Give me a one word, yes or no answer."
  const chatResponse = await client.chat({
    model: 'mistral-tiny',
    messages: [{role: 'user', content: message}],
  });

  console.log(chatResponse.choices[0].message.content);

  let textRepsonse = chatResponse.choices[0].message.content;
  textRepsonse = textRepsonse.split(' ');
  const result = textRepsonse[0].substring(0, textRepsonse[0].length - 1);

  console.log('Result: ', result);

  if (result.toLowerCase() === 'no') {
    process.exit(0);
  } else {
    process.exit(1);
  }
}

function run_typechecker(program, spec) {
  // check args
  // if (process.argv.length < 4) {
  //   console.log('Needs more args, usage: typechecker <program> <spec>');
  //   process.exit(1);
  // }

  // process filepaths
  // const programFile = process.argv[2];
  // const specFile = process.argv[3];
  // const absProgramFile = path.resolve(process.cwd(), programFile);
  // const absSpecFile = path.resolve(process.cwd(), specFile);

  // parse args into strings
  // const program = fs.readFileSync(absProgramFile).toString('utf-8');
  // const spec = fs.readFileSync(absSpecFile).toString('utf-8');

  // create client
  const apiKey = process.env.MISTRAL_API_KEY || 'your_api_key';
  console.log(apiKey);
  const client = new MistralClient(apiKey);

  // typecheck
  return typecheck(program, spec, client);
}

const PORT = 6969;
const app = express();
app.use(express.json());

app.post('/typechecker', async (req, res) => {
  const programBody = req.body.program;
  const specBody = req.body.spec;
  console.log(programBody);
  const result = await run_typechecker(programBody, specBody);
  res.end(result.toString());
});

app.get('/test', (req, res) => {
  res.end("Hello World");
})

app.listen(PORT, "0.0.0.0", () => {
    console.log(`Server is running on Port ${PORT}`)
})
