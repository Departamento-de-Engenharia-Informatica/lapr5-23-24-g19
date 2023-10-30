import * as env from './common'
import { expect } from 'chai'
import { parseInt } from 'lodash'

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

async function mkPassageFloor() {
    const bPayload = mkBuilding()
    await post('/buildings/', bPayload)

    const fPayload = mkFloor()
    fPayload.floorNumber *= env.randomLen(1000, 4000)

    await post(`/buildings/${bPayload.code}/floors`, fPayload, (tx, rx) => {
        tx.buildingCode = bPayload.code
        expect(rx).to.deep.equal(tx)
    })

    return {
        buildingCode: bPayload.code,
        floorNumber: fPayload.floorNumber
    }
}

async function getPassages(b1: string, b2: string) {
    const body = await fetch(`${env.prefix}/passages/?building1=${b1}&building2=${b2}`)

    if (!body.ok) {
        return 0
    }

    const elevators = await body.json()
    expect(elevators).instanceof(Array)

    return elevators.length
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

describe('create passage', () => {

    it('does not allow repeats', async () => {
        const floor1 = await mkPassageFloor()
        const floor2 = await mkPassageFloor()

        const len1 = await getPassages(floor1.buildingCode, floor2.buildingCode)

        const payload = { floor1, floor2 }

        await post(`/passages`, payload, (tx, rx) => {
            expect(rx).to.deep.equal(tx)
        })

        const len2 = await getPassages(floor1.buildingCode, floor2.buildingCode)
        expect(len2).to.equal(len1 + 1)

        const len3 = await getPassages(floor1.buildingCode, floor2.buildingCode)
        expect(len3).to.equal(len2)
    })
})
