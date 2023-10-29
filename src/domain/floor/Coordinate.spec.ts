import { assert } from 'chai'
import { Coordinates } from './Coordinates'

describe('Coordinates', () => {
    it('should create valid coordinates', () => {
        const x = 10
        const y = 20

        const result = Coordinates.create(x, y)

        assert.isOk(result.isSuccess)
        const coordinates = result.getValue()
        assert.equal(coordinates.x, x)
        assert.equal(coordinates.y, y)
    })

    it('should fail when creating coordinates with null or undefined x', () => {
        const y = 20

        const result = Coordinates.create(null, y)

        assert.isNotOk(result.isSuccess)
    })

    it('should fail when creating coordinates with null or undefined y', () => {
        const x = 10

        const result = Coordinates.create(x, undefined)

        assert.isNotOk(result.isSuccess)
    })

    it('should return the correct values from getValues()', () => {
        const x = 10
        const y = 20

        const coordinates = Coordinates.create(x, y).getValue()
        const values = coordinates.getValues()

        assert.deepEqual(values, [x, y])
    })
})
