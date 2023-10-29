import { expect } from 'chai'

// export declare var fetch

export const port = 4000
export const prefix = `http://localhost:${port}/api`;

export const lower = 'qwertyuiopasdfghjklzxcvbnm'
export const upper = lower.toUpperCase()
export const num = '1234567890'

export const alnum = lower + upper + num
export const alnumSpc = alnum + ' '

export function randomLen(max: number, min = 1) {
    return Math.floor(Math.random() * (max - min + 1) + min)
}

export function makeid(charset: string, length: number) {
    let result = '';

    const characters = charset
    const charactersLength = characters.length;

    let counter = 0;

    while (counter < length) {
        result += characters.charAt(Math.floor(Math.random() * charactersLength));
        counter += 1;
    }
    return result;
}

// export async function post(
//     endpoint: string,
//     data: Object,
//     cmp?: (tx: any, rx: any) => void) {

//     const body = await fetch(`${prefix}${endpoint}`, {
//         method: 'POST',
//         headers: { "Content-type": "application/json" },
//         body: JSON.stringify(data)
//     })

//     if (body.ok) {
//         const received = await body.json()
//         if (cmp) {
//             cmp(data, received)
//         } else {
//             expect(received).to.deep.equal(data)
//         }
//     }
// }
