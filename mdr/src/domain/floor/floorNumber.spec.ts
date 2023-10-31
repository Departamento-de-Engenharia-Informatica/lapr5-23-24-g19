import { assert } from 'chai'
import { describe, it } from 'mocha'

import { FloorNumber } from './floorNumber'

describe('Floor Number', () => {
    it('should have a valid floor number', () => {
        let floor = FloorNumber.create(3)
        assert.isOk(floor.isSuccess)

        floor = FloorNumber.create(0)
        assert.isOk(floor.isSuccess)

        floor = FloorNumber.create(-3)
        assert.isOk(floor.isSuccess)

        floor = FloorNumber.create(undefined)
        assert.isNotOk(floor.isSuccess)
    })
})
