import { assert } from 'chai'
import { describe, it } from 'mocha'

import { BuildingName } from './name'

describe('Building Name', () => {
    it('should be no longer than 50 characters', () => {
        let name = BuildingName.create('B001')
        assert.isOk(name.isSuccess)

        name = BuildingName.create(
            'A building name with more than 50 characters,' +
                'A building name with more than 50 characters,' +
                'A building name with more than 50 characters,' +
                'A building name with more than 50 characters,' +
                'A building name with more than 50 characters,' +
                'A building name with more than 50 characters,' +
                'A building name with more than 50 characters.',
        )
        assert.isNotOk(name.isSuccess)
    })

    it('cannot be null', () => {
        const name = BuildingName.create(null)
        assert.isNotOk(name.isSuccess)
    })

    it('cannot be undefined', () => {
        const name = BuildingName.create(undefined)
        assert.isNotOk(name.isSuccess)
    })

    it('allows blanks', () => {
        const name = BuildingName.create('    ')
        assert.isOk(name.isSuccess)
    })
})
