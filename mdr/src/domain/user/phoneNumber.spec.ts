import { expect } from 'chai'
import { describe, it, after } from 'mocha'
import config from '../../../config'

import { PhoneNumber } from './phoneNumber'

const maxPhoneLen = config.phoneNumberLength

after(() => {
    config.phoneNumberLength = maxPhoneLen
})

describe('User phone number', () => {
    beforeEach(() => {
        config.phoneNumberLength = maxPhoneLen
    })

    it('cannot be empty', () => {
        let email = PhoneNumber.create('')
        expect(email.isSuccess).to.be.false
    })

    it('cannot exceed allowed phone length', () => {
        config.phoneNumberLength = 9
        const num = '9125552812'

        const phone = PhoneNumber.create(num)

        expect(phone.isSuccess).to.be.false
    })

    it('cannot be under allowed phone length', () => {
        config.phoneNumberLength = 9
        const num = '9125552'

        const phone = PhoneNumber.create(num)

        expect(phone.isSuccess).to.be.false
    })

    it('must be of an allowed length', () => {
        config.phoneNumberLength = 9
        const num = '912555222'

        const phone = PhoneNumber.create(num)

        expect(phone.isSuccess).to.be.true
    })
})
