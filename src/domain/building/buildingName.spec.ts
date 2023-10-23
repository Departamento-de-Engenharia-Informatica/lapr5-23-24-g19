import { assert } from 'chai'
import { describe, it } from 'mocha'

import { BuildingName } from './buildingName'

describe('Building Name', () => {
    it('should not exceed the maximum length of 50 characters', () => {
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

        name = BuildingName.create()
        assert.isOk(name.isSuccess)
    })
})
