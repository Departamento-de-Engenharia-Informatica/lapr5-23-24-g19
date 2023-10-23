import { assert } from 'chai'
import { describe, it } from 'mocha'

import { BuildingDescription } from './description'

describe('Building Description', () => {
    it('must be alphanumeric, not be null or undefined and should have a maximum length of 255 characters', () => {
        let desc = BuildingDescription.create(null)
        assert.isNotOk(desc.isSuccess)

        desc = BuildingDescription.create(undefined)
        assert.isNotOk(desc.isSuccess)

        desc = BuildingDescription.create(
            'This is a building description with more than 255 characters,This is a building description with more than 255 characters,' +
                'This is a building description with more than 255 characters,' +
                'This is a building description with more than 255 characters,' +
                'This is a building description with more than 255 characters,' +
                'This is a building description with more than 255 characters,' +
                'This is a building description with more than 255 characters,' +
                'This is a building description with more than 255 characters,' +
                'This is a building description with more than 255 characters,' +
                'This is a building description with more than 255 characters',
        )
        assert.isNotOk(desc.isSuccess)

        desc = BuildingDescription.create('BuildingXpto')
        assert.isOk(desc.isSuccess)
    })
})
