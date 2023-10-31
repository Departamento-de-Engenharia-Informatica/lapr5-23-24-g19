import { assert } from 'chai'
import { describe, it } from 'mocha'

import { BuildingDescription } from './description'

describe('Building Description', () => {
    it('should be at most 255 characters', () => {
        let description = BuildingDescription.create('BUILDINGA')
        assert.isOk(description.isSuccess)

        description = BuildingDescription.create('buildingA')
        assert.isOk(description.isSuccess)

        description = BuildingDescription.create('BuildingForDei')
        assert.isOk(description.isSuccess)

        description = BuildingDescription.create('DEIISEP23')
        assert.isOk(description.isSuccess)

        description = BuildingDescription.create('DeCivilA%')
        assert.isOk(description.isSuccess)

        description = BuildingDescription.create('#!4$%')
        assert.isOk(description.isSuccess)

        description = BuildingDescription.create('!')
        assert.isOk(description.isSuccess)

        description = BuildingDescription.create('#()=>')
        assert.isOk(description.isSuccess)

        description = BuildingDescription.create(
            'This is a building description with more than 255 characters' +
                'This is a building description with more than 255 characters,' +
                'This is a building description with more than 255 characters,' +
                'This is a building description with more than 255 characters,' +
                'This is a building description with more than 255 characters,' +
                'This is a building description with more than 255 characters,' +
                'This is a building description with more than 255 characters,' +
                'This is a building description with more than 255 characters,' +
                'This is a building description with more than 255 characters,' +
                'This is a building description with more than 255 characters',
        )
        assert.isNotOk(description.isSuccess)

        description = BuildingDescription.create('BuildingXpto')
        assert.isOk(description.isSuccess)
    })

    it('allows spaces in-between', () => {
        let descripton = BuildingDescription.create('DEI building for 1st years')
        assert.isOk(descripton.isSuccess)

        descripton = BuildingDescription.create('LEEC building for 1st and 3rd years')
        assert.isOk(descripton.isSuccess)

        descripton = BuildingDescription.create('External Relationships')
        assert.isOk(descripton.isSuccess)

        descripton = BuildingDescription.create('Internal Affairs')
        assert.isOk(descripton.isSuccess)
    })

    it('cannot be null', () => {
        const code = BuildingDescription.create(null)
        assert.isNotOk(code.isSuccess)
    })

    it('cannot be undefined', () => {
        const code = BuildingDescription.create(undefined)
        assert.isNotOk(code.isSuccess)
    })
})
