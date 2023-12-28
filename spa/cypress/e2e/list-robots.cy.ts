function loginViaAuth0Ui(username: string, password: string) {
    cy.origin(
        Cypress.env('auth_domain'),
        { args: { username, password } },
        ({ username, password }) => {
            cy.get('input#1-email').type(username)
            cy.get('input#1-password').type(password, { log: false })
            cy.get('button[type="submit"]')
                .should('be.visible')
                .should('contain.text', 'Log In')
                .should('not.be.disabled')
                .should('not.be.hidden')
                .click()
        },
    )
}

describe('List Robots e2e tests', () => {
    beforeEach(() => {
        cy.intercept('GET', 'http://localhost:4000/api/robots', {
            body: [
                {
                    code: 'R1',
                    nickname: 'Robot1',
                    typeCode: 'T1',
                    serialNumber: 'SN1',
                    description: 'Robot 1 Description',
                },
                {
                    code: 'R2',
                    nickname: 'Robot2',
                    typeCode: 'T2',
                    serialNumber: 'SN2',
                    description: 'Robot 2 Description',
                },
                {
                    code: 'R3',
                    nickname: 'Robot3',
                    typeCode: 'T3',
                    serialNumber: 'SN3',
                    description: 'Robot 3 Description',
                },
                {
                    code: 'R14',
                    nickname: 'Robot14',
                    typeCode: 'T14',
                    serialNumber: 'SN14',
                    description: 'Robot 14 Description',
                },
            ],
        }).as('getRobots')
        window.localStorage.setItem('USER_ROLES', 'ADM')

        cy.visit('/fleet' + '/robots/list')
        const log = Cypress.log({
            displayName: 'AUTH0 LOGIN',
            message: [`🔐 Authenticating | ${Cypress.env('auth_username')}`],
            // @ts-ignore
            autoEnd: false,
        })
        log.snapshot('before')

        loginViaAuth0Ui(Cypress.env('auth_username'), Cypress.env('auth_password'))

        log.snapshot('after')
        log.end()
    })

    it('has the correct title', () => {
        cy.title().should('equal', 'List all robots in the fleet')
    })

    it('should have an empty robot list', () => {
        cy.get('.robot-card').should('not.exist')
    })

    it('should display robots', () => {
        cy.wait('@getRobots')

        cy.get('.robot-card').should('exist')
        cy.get('.robot-card').should('contain.text', 'Code: R1')
        cy.get('.robot-card').should('contain.text', 'Nickname: Robot1')
        cy.get('.robot-card').should('contain.text', 'Typecode: T1')
        cy.get('.robot-card').should('contain.text', 'Serialnumber: SN1')
        cy.get('.robot-card').should('contain.text', 'Description: Robot 1 Description')

        cy.get('.robot-card').should('contain.text', 'Code: R2')
        cy.get('.robot-card').should('contain.text', 'Nickname: Robot2')
        cy.get('.robot-card').should('contain.text', 'Typecode: T2')
        cy.get('.robot-card').should('contain.text', 'Serialnumber: SN2')
        cy.get('.robot-card').should('contain.text', 'Description: Robot 2 Description')

        cy.get('.robot-card').should('contain.text', 'Code: R3')
        cy.get('.robot-card').should('contain.text', 'Nickname: Robot3')
        cy.get('.robot-card').should('contain.text', 'Typecode: T3')
        cy.get('.robot-card').should('contain.text', 'Serialnumber: SN3')
        cy.get('.robot-card').should('contain.text', 'Description: Robot 3 Description')

        cy.get('.robot-card').should('contain.text', 'Code: R14')
        cy.get('.robot-card').should('contain.text', 'Nickname: Robot14')
        cy.get('.robot-card').should('contain.text', 'Typecode: T14')
        cy.get('.robot-card').should('contain.text', 'Serialnumber: SN14')
        cy.get('.robot-card').should('contain.text', 'Description: Robot 14 Description')
    })
})
