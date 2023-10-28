import { assert } from 'chai'
import { describe, it } from 'mocha'
import { RobotTypeBrand } from './robotTypeBrand'

describe('RobotTypeBrand', () => {
    it('should create a valid robot type brand', () => {
        const validBrandValue = 'RoboTech'
        const robotTypeBrand = RobotTypeBrand.create(validBrandValue)

        assert.isOk(robotTypeBrand.isSuccess)
    })

    it('should fail when creating a robot type brand with null or undefined value', () => {
        const robotTypeBrand1 = RobotTypeBrand.create(null)
        assert.isNotOk(robotTypeBrand1.isSuccess)

        const robotTypeBrand2 = RobotTypeBrand.create(undefined)
        assert.isNotOk(robotTypeBrand2.isSuccess)
    })

    it('should fail when the brand exceeds the maximum length', () => {
        const longBrand = 'A'.repeat(51) // Exceeds the maximum length of 50 characters
        const robotTypeBrand = RobotTypeBrand.create(longBrand)

        assert.isNotOk(robotTypeBrand.isSuccess)
    })

    it('should trim leading and trailing whitespace from the value', () => {
        const brandValue = '   RoboTech   '
        const robotTypeBrand = RobotTypeBrand.create(brandValue)

        assert.isOk(robotTypeBrand.isSuccess)
        assert.equal(robotTypeBrand.getValue().value, 'RoboTech')
    })
})
