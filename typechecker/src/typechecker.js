import MistralClient from '@mistralai/mistralai';
import * as dotenv from 'dotenv';
dotenv.config();

async function typecheck(program, spec, client) {
  let message = "The spec of the programming language is:\n" + spec + " The program is as follows:\n " + program + " \n Is the program correct given the spec? Give a one word answer." 

  const chatResponse = await client.chat({
      model: 'mistral-tiny',
      messages: [{role: 'user', content: message}],
    });
  
  let textRepsonse = chatResponse.choices[0].message.content;
  textRepsonse = textRepsonse.split(' ');
  const result = textRepsonse[0].split('.')[0];

  console.log('Chat:', result);
}

async function doesHalt(program, spec, client) {
  let message = "The spec for the programming language is:\n" + spec + "\nDoes the following program halt given the spec?:\n " + program + "Give me a one word, yes or no answer."
  const chatResponse = await client.chat({
    model: 'mistral-tiny',
    messages: [{role: 'user', content: message}],
  });

  let textRepsonse = chatResponse.choices[0].message.content;
  textRepsonse = textRepsonse.split(' ');
  const result = textRepsonse[0].split('.')[0];

  console.log('Chat:', result);
}

const apiKey = process.env.MISTRAL_API_KEY || 'your_api_key';
const client = new MistralClient(apiKey);
typecheck('a', 'all lines must end with a ;', client);