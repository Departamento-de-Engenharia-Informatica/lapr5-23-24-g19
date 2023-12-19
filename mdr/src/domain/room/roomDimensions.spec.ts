import { assert } from 'chai'
import { describe, it } from 'mocha'
import { RoomDimensions } from './roomDimensions'

describe('Room Dimensions', () => {
    it('should create valid dimensions', () => {
        const validDimensions = {
            length: 5,
            width: 3,
        }

        const roomDimensions = RoomDimensions.create(
            validDimensions.length,
            validDimensions.width,
        )

        assert.isOk(roomDimensions.isSuccess)
    })

    it('should fail when dimensions are null or undefined', () => {
        const roomDimensions1 = RoomDimensions.create(null, 3)
        assert.isNotOk(roomDimensions1.isSuccess)

        const roomDimensions2 = RoomDimensions.create(5, undefined)
        assert.isNotOk(roomDimensions2.isSuccess)
    })

    it('should fail when dimensions are less than 1 unit', () => {
        const invalidDimensions = {
            length: 0,
            width: -1,
        }

        const roomDimensions = RoomDimensions.create(
            invalidDimensions.length,
            invalidDimensions.width,
        )

        assert.isNotOk(roomDimensions.isSuccess)
    })
})
