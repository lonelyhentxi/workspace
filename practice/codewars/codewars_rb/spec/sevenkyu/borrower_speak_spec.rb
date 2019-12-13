# frozen_string_literal: true

require 'rspec'
require 'sevenkyu/borrower_speak'

describe 'Test borrow speak' do
  it 'should only contains alphanum' do
    expect(borrow('WhAt! FiCK! DaMn CAke?')).to eq( 'whatfickdamncake')
    expect(borrow('THE big PeOpLE Here!!')).to eq( 'thebigpeoplehere')
    expect(borrow('i AM a TINY BoY!!')).to eq( 'iamatinyboy')
    expect(borrow('DOnt YOU SAY THAT!')).to eq( 'dontyousaythat')
    expect(borrow('borrow BORROW BoRrOw IT!?')).to eq( 'borrowborrowborrowit')
  end
end
