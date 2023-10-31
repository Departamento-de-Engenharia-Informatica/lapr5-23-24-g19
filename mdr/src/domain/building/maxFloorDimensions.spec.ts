import { assert } from 'chai'
import { describe, it } from 'mocha'

import { MaxFloorDimensions } from './maxFloorDimensions'

describe('Max Floor Dimensions', () => {
    it('should have a positive length', () => {
        let dim = MaxFloorDimensions.create(-10, 40)
        assert.isNotOk(dim.isSuccess)

        dim = MaxFloorDimensions.create(undefined, 40)
        assert.isNotOk(dim.isSuccess)

        dim = MaxFloorDimensions.create(null, 40)
        assert.isNotOk(dim.isSuccess)

        dim = MaxFloorDimensions.create(0, 40)
        assert.isNotOk(dim.isSuccess)

        dim = MaxFloorDimensions.create(24, 40)
        assert.isOk(dim.isSuccess)
    })

    it('should have a positive width', () => {
        let dim = MaxFloorDimensions.create(10, -30)
        assert.isNotOk(dim.isSuccess)

        dim = MaxFloorDimensions.create(24, undefined)
        assert.isNotOk(dim.isSuccess)

        dim = MaxFloorDimensions.create(24, null)
        assert.isNotOk(dim.isSuccess)

        dim = MaxFloorDimensions.create(24, 0)
        assert.isNotOk(dim.isSuccess)

        dim = MaxFloorDimensions.create(109, 67)
        assert.isOk(dim.isSuccess)
    })

    it('should have positive values for length and width', () => {
        let dimensions = MaxFloorDimensions.create(10, 20)
        assert.isOk(dimensions.isSuccess)

        dimensions = MaxFloorDimensions.create(0, 20)
        assert.isNotOk(dimensions.isSuccess)

        dimensions = MaxFloorDimensions.create(10, 0)
        assert.isNotOk(dimensions.isSuccess)

        dimensions = MaxFloorDimensions.create(-10, 20)
        assert.isNotOk(dimensions.isSuccess)
    })
})
