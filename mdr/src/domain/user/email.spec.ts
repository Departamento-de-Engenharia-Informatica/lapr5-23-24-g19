import { expect } from 'chai'
import { describe, it, after } from 'mocha'
import config from '../../../config'

import { Email } from './email'

const defaultDomains = config.validEmailDomains

after(() => {
    config.validEmailDomains = defaultDomains
})

describe('User email', () => {
    beforeEach(() => {
        config.validEmailDomains = defaultDomains
    })

    it('cannot be empty', () => {
        let email = Email.create('')
        expect(email.isSuccess).to.be.false

        email = Email.create('    ')
        expect(email.isSuccess).to.be.false
    })

    it('cannot have two @ signs', () => {
        config.validEmailDomains = ['isep.ipp.pt']
        const addr = 'hello@123' + '@' + config.validEmailDomains[0]

        const email = Email.create(addr)
        expect(email.isSuccess).to.be.false
    })

    it('must have an @ sign', () => {
        config.validEmailDomains = ['isep.ipp.pt']
        const addr = 'hello' + '@' + config.validEmailDomains[0]

        const email = Email.create(addr)
        expect(email.isSuccess).to.be.true
    })

    it('must belong to an allowed domain name', () => {
        config.validEmailDomains = ['isep.ipp.pt', 'dei.isep.ipp.pt']

        let addr = 'mzc@isep.ipp.pt'

        let email = Email.create(addr)
        expect(email.isSuccess).to.be.true

        addr = 'ajs@isep.ipp.pt'

        email = Email.create(addr)
        expect(email.isSuccess).to.be.true

        addr = 'miguel@google.com'

        email = Email.create(addr)
        expect(email.isSuccess).to.be.false
    })
})
