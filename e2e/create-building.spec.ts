import * as env from './common'
import { expect } from 'chai'

declare var fetch

function mkdata() {
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

async function getBuildings() {

    const body = await fetch(`${env.prefix}/buildings`)

    if (!body.ok) {
        return 0
    }

    const buildingsInit = await body.json()
    expect(buildingsInit).instanceof(Array)

    return buildingsInit.length
}

async function postBuilding(data: Object) {

    const body = await fetch(`${env.prefix}/buildings`, {
        method: 'POST',
        headers: { "Content-type": "application/json" },
        body: JSON.stringify(data)
    })

    if (body.ok) {
        const building = await body.json()
        expect(building).to.deep.equal(data)
    }
}

describe('create building', () => {

    it('does not allow repeats', async () => {
        const len1 = await getBuildings()

        const payload = mkdata()

        await postBuilding(payload)

        const len2 = await getBuildings()

        expect(len2).to.equal(len1 + 1)

        await postBuilding(payload)

        const len3 = await getBuildings()

        expect(len3).to.equal(len2)
    })
})
