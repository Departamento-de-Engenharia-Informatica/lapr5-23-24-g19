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

function mkElevator(bFloors: number[]) {

    // const floors = bFloors.map(bf => parseInt(bf))
    const floors = bFloors

    const brand = env.makeid(env.alnumSpc, env.randomLen(50)).trim()
    const model = env.makeid(env.alnumSpc, env.randomLen(50)).trim()
    const serialNumber = env.makeid(env.alnumSpc, env.randomLen(50)).trim()
    const description = env.makeid(env.alnumSpc, env.randomLen(255)).trim()

    return {
        floors,
        brand,
        model,
        serialNumber,
        description,
    }
}

async function getElevators(buildingCode: string) {
    const body = await fetch(`${env.prefix}/buildings/${buildingCode}/elevators`)

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

describe('create elevator', () => {

    it('does not allow repeats', async () => {
        const bPayload = mkBuilding()
        await post('/buildings/', bPayload)

        const nFloors = env.randomLen(40)
        const floors: number[] = []

        for (let i = 0; i < nFloors; i++) {
            const fPayload = mkFloor()
            fPayload.floorNumber *= env.randomLen(1000, 4000)
            // console.log(fPayload.floorNumber)
            await post(`/buildings/${bPayload.code}/floors`, fPayload, (tx, rx) => {
                tx.buildingCode = bPayload.code
                expect(rx).to.deep.equal(tx)
            })

            floors.push(fPayload.floorNumber)
        }

        const len1 = await getElevators(bPayload.code)

        const ePayload = mkElevator(floors)

        await post(`/buildings/${bPayload.code}/elevators`, ePayload, (tx, rx) => {
            tx.buildingId = bPayload.code
            tx.identifier = rx.identifier

            expect(rx).to.deep.equal(tx)
        })

        const len2 = await getElevators(bPayload.code)
        expect(len2).to.equal(len1 + 1)

        const len3 = await getElevators(bPayload.code)
        expect(len3).to.equal(len2)
    })
})
