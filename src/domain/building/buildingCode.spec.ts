import { assert } from 'chai'
import { describe, it } from 'mocha'

import { BuildingCode } from './buildingCode'

describe('Building Code', () => {
    it('Code must contain at most 5 characters, letters and numbers and possibly with spaces in-between', () => {

        const code = BuildingCode.create('AB123')
        assert.isOk(code.isSuccess)

        const code1 = BuildingCode.create('A B1 2 3')
        assert.isNotOk(code1.isSuccess)

        const code2 = BuildingCode.create('ABC 12345')
        assert.isNotOk(code2.isSuccess)

        const code3 = BuildingCode.create('ABC@123')
        assert.isNotOk(code3.isSuccess)

        const code4 = BuildingCode.create(undefined)
        assert.isNotOk(code4.isSuccess)
    })
})
