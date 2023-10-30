import * as env from './common'
import { expect } from 'chai'

declare var fetch
function mkBuilding() {
    const code = env.makeid(env.alnum, env.randomLen(5)).trim()
    const name = env.makeid(env.alnumSpc, env.randomLen(50)).trim()
    const description = env.makeid(env.alnumSpc, env.randomLen(255)).trim()
    const maxFloorDimensions = {
        length: env.randomLen(150),
        width: env.randomLen(127)
    }

    return {
        code,
        name,
        description,
        maxFloorDimensions
    }
}

function mkFloor() {
    const floorNumber = parseInt(env.makeid(env.num, 3))
    const description = env.makeid(env.alnumSpc, env.randomLen(250)).trim()

    return {
        floorNumber,
        description,
    }
}

async function getFloors(buildingCode: string) {

    const body = await fetch(`${env.prefix}/buildings/${buildingCode}/floors`)

    if (!body.ok) {
        return 0
    }

    const floors = await body.json()
    expect(floors).instanceof(Array)

    return floors.length
}

async function post(endpoint: string, data: Object, cmp?: (tx, rx) => void) {

    const body = await fetch(`${env.prefix}${endpoint}`, {
        method: 'POST',
        headers: { "Content-type": "application/json" },
        body: JSON.stringify(data)
    })

    if (body.ok) {
        const received = await body.json()
        if (cmp) {
            cmp(data, received)
        } else {
            expect(received).to.deep.equal(data)
        }
    }
}

describe('create floor', () => {

    it('does not allow repeats', async () => {
        const bPayload = mkBuilding()
        await post('/buildings/', bPayload)

        const len1 = await getFloors(bPayload.code)

        const fPayload = mkFloor()
        await post(`/buildings/${bPayload.code}/floors`, fPayload, (tx, rx) => {
            tx.buildingCode = bPayload.code
            expect(rx).to.deep.equal(tx)
        })

        const len2 = await getFloors(bPayload.code)
        expect(len2).to.equal(len1 + 1)

        const len3 = await getFloors(bPayload.code)
        expect(len3).to.equal(len2)
    })
})
