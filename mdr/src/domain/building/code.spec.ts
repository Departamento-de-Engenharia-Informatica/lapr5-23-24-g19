import { assert } from 'chai'
import { describe, it } from 'mocha'

import { BuildingCode } from './code'

describe('Building Code', () => {
    it('should be alphanumeric', () => {
        let code = BuildingCode.create('BUILA')
        assert.isOk(code.isSuccess)

        code = BuildingCode.create('qidi')
        assert.isOk(code.isSuccess)

        code = BuildingCode.create('DeiA')
        assert.isOk(code.isSuccess)

        code = BuildingCode.create('DecA2')
        assert.isOk(code.isSuccess)

        code = BuildingCode.create('DecA%')
        assert.isNotOk(code.isSuccess)

        code = BuildingCode.create('#!4$%')
        assert.isNotOk(code.isSuccess)

        code = BuildingCode.create('!')
        assert.isNotOk(code.isSuccess)

        code = BuildingCode.create('#()=>')
        assert.isNotOk(code.isSuccess)
    })

    it('should be at most 5 characters', () => {
        let code = BuildingCode.create('AB123')
        assert.isOk(code.isSuccess)

        code = BuildingCode.create('AB12')
        assert.isOk(code.isSuccess)

        code = BuildingCode.create('AB1')
        assert.isOk(code.isSuccess)

        code = BuildingCode.create('AB')
        assert.isOk(code.isSuccess)

        code = BuildingCode.create('A')
        assert.isOk(code.isSuccess)

        code = BuildingCode.create('')
        assert.isNotOk(code.isSuccess)

        code = BuildingCode.create('AB1234')
        assert.isNotOk(code.isSuccess)
    })

    it('allows spaces in-between', () => {
        let code = BuildingCode.create('DEI A')
        assert.isOk(code.isSuccess)

        code = BuildingCode.create('DE  A')
        assert.isOk(code.isSuccess)
    })

    it('cannot be null', () => {
        const code = BuildingCode.create(null)
        assert.isNotOk(code.isSuccess)
    })

    it('cannot be undefined', () => {
        const code = BuildingCode.create(undefined)
        assert.isNotOk(code.isSuccess)
    })
})
