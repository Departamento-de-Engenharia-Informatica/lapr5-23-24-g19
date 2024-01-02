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

describe('Approve reject task e2e tests', () => {
    const body = [
        {
            id: { value: '585ef2dc-7626-489d-9b13-87dc5314394d' },
            jobType: 1,
            state: 0,
            email: 'user@isep.ipp.pt',
            location: {
                startingPoint: { buildingCode: 'A', floorNumber: 1 },
                endingPoint: { buildingCode: 'B', floorNumber: 2 },
            },
            pickupContact: {
                name: 'John Doe',
            },
        },
        {
            id: { value: '6a3c8b27-d369-432d-8193-28449aaf4ffb' },
            jobType: 0,
            state: 0,
            email: 'anotheruser@isep.ipp.pt',
            location: {
                startingPoint: { buildingCode: 'C', floorNumber: 3 },
                endingPoint: { buildingCode: 'C', floorNumber: 3 },
            },
            surveillanceContact: {
                name: 'Jane Doe',
            },
        },
    ]
    beforeEach(() => {
        cy.intercept('GET', 'http://localhost:4000/api/task?status=Pending', {
            body: body,
        }).as('getTasks')
        window.localStorage.setItem('USER_ROLES', 'TKM')

        cy.visit('/task' + '/approve-reject')
        const log = Cypress.log({
            displayName: 'AUTH0 LOGIN',
            message: [`🔐 Authenticating | ${Cypress.env('auth_username')}`],
            autoEnd: false,
        })
        log.snapshot('before')

        loginViaAuth0Ui(Cypress.env('auth_username'), Cypress.env('auth_password'))

        log.snapshot('after')
        log.end()
    })

    it('has the correct title', () => {
        cy.title().should('equal', 'Approve/Reject task');
    });

    it('should have an empty task list', () => {
        cy.get('.building-card').should('not.exist');
    });

    it('should display pending approval tasks', () => {
        cy.wait('@getTasks');

        const [loc1, loc2] = body.map(b => b.location)
        cy.get('.building-card').should('exist');
        cy.get('.building-card').should('contain.text', 'Delivery');
        cy.get('.building-card').should(
            'contain.text',
            `${loc1.startingPoint.buildingCode}–${loc1.startingPoint.floorNumber} → ${loc1.endingPoint.buildingCode}–${loc1.endingPoint.floorNumber}`
        );
        cy.get('.building-card').should(
            'contain.text',
            body[0].email,
        );
        cy.get('.building-card').should(
            'contain.text',
            body[0].pickupContact?.name
        );

        cy.get('.building-card').should('contain.text', 'Surveillance');
        cy.get('.building-card').should(
            'contain.text',
            `${loc2.startingPoint.buildingCode}–${loc2.startingPoint.floorNumber}`
        );
        cy.get('.building-card').should(
            'contain.text',
            body[1].email,
        );
        cy.get('.building-card').should(
            'contain.text',
            body[1].surveillanceContact?.name
        );
    });

    // it('should be able to click approve task', () => {
    //     cy.intercept('PATCH', 'http://localhost:4000/api/task/*', (req) => {
    //         console.log('Approve task request intercepted:', req)
    //         req.reply({
    //             status: 200,
    //             body: { success: true },
    //         })
    //     }).as('approveTask')

    //     cy.wait('@getTasks')

    //     // Task card is initially present in the list
    //     cy.get('.building-card').should('contain.text', 'A–1 → B–2')

    //     const taskCardToApprove = cy.get('.building-card').contains('div', 'A–1 → B–2')
    //     taskCardToApprove.find('button[title="Approve task"]').click()

    //     cy.wait('@approveTask')

    //     // Task card is no longer present in the list after approval
    //     taskCardToApprove.should('not.exist')
    // })
})
