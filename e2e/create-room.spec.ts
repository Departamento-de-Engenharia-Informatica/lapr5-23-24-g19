import * as env from './common'
import { expect } from 'chai'
import { parseInt, toString } from 'lodash'
import { undefined } from 'webidl-conversions'

declare var fetch
function mkBuilding() {
    const code = env.makeid(env.alnum, env.randomLen(5)).trim()
    const name = env.makeid(env.alnumSpc, env.randomLen(50)).trim()
    const description = env.makeid(env.alnumSpc, env.randomLen(255)).trim()
    const maxFloorDimensions = {
        length: env.randomLen(150),
        width: env.randomLen(127),
    }

    return {
        code,
        name,
        description,
        maxFloorDimensions,
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

async function mkRoom() {
    const bPayload = mkBuilding()
    await post('/buildings/', bPayload)

    const fPayload = mkFloor()
    fPayload.floorNumber *= env.randomLen(1000, 4000)

    await post(`/buildings/${bPayload.code}/floors`, fPayload, (tx, rx) => {
        tx.buildingCode = bPayload.code
        expect(rx).to.deep.equal(tx)
    })

    const name = env.makeid(env.alnumSpc, env.randomLen(50)).trim()
    const description = env.makeid(env.alnumSpc, env.randomLen(250)).trim()
    const category = 'GABINETE'
    const dimensions = {
        length: env.randomLen(20),
        width: env.randomLen(15),
    }
    const positions = {
        x: env.randomLen(20),
        y: env.randomLen(15),
    }

    return {
        buildingCode: bPayload.code,
        floorNumber: fPayload.floorNumber,

        name,
        description,
        category,
        dimensions,
        positions,
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
        headers: { 'Content-type': 'application/json' },
        body: JSON.stringify(data),
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

describe('create room', () => {
    it('does not allow repeats', async () => {
        const room = await mkRoom()
        const payload = { ...room, buildingCode: undefined, floorNumber: undefined }

        // List rooms not impl because we're a 4 element group
        //
        // const len = await getRooms(room.buildingCode, room.floorNumber)

        await post(`/${room.buildingCode}/floors/${room.floorNumber}/rooms`, payload, (_, rx) => {
            rx.floorNumber = toString(rx.floorNumber)
            expect(rx).to.deep.equal(room)
        })

        // const len2 = await getRooms(room.buildingCode, room.floorNumber)
        // expect(len2).to.equal(len1 + 1)

        // const len3 = await getRooms(room.buildingCode, room.floorNumber)
        // expect(len3).to.equal(len2)
    })
})
