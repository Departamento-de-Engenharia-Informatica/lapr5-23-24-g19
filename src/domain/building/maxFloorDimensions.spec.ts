import { assert } from 'chai'
import { describe, it } from 'mocha'

import { MaxFloorDimensions } from './maxFloorDimensions'

describe('Max Floor Dimensions', () => {
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
