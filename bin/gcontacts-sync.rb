#!/usr/bin/env ruby
 
# Google Contact Photos - Gravatar Importer
# Written by Ashley Angell
# http://ashleyangell.com
# Licenced under Creative Commons with Attribution
 
require "rubygems"
require "gdata"
require "rexml/document"
require "digest/md5"
require "net/http"
include REXML
 
none = 'd5fe5cbcc31cff5f8ac010db72eb000c'
user = ARGV[0]
pass = ARGV[1]
 
client = GData::Client::Contacts.new
client.clientlogin(user, pass)
data = client.get("https://www.google.com/m8/feeds/contacts/#{user}/full?max-results=10000")
myxml = Document.new data.body
p "contacts"
puts "-"*70
i = 0
myxml.each_element("feed/entry") do |e|
  	begin
    	gd = e.elements['gd:email']
    	if !gd.nil?
      		email = gd.attributes['address'].downcase
      		hash = Digest::MD5.hexdigest(email)
      		image_src = "http://www.gravatar.com/avatar/#{hash}"
      		nil_image = false
      		image_element = e.get_elements("link[@rel='http://schemas.google.com/contacts/2008/rel#photo']")[0]
      		if !image_element.nil? and image_element.attributes['gd:etag'].nil?
        		data = nil
        		md5 = nil
        		Net::HTTP.start(URI.parse(image_src).host) do |http|
          			resp = http.get(URI.parse(image_src).path)
          			data = resp.body
          			md5 = Digest::MD5.hexdigest(data)
          			File.open("#{email}.png", 'w') do |f|
            			f.puts data if md5 != none
          			end
        		end
        		md5 = Digest::MD5.hexdigest(data)
        		if md5 != none
          			puts "#{email} > #{image_src}"
          			client.put_file(image_element.attributes['href'], "#{email}.png", 'image/png')
          			i = i + 1
        		else
          			puts "#{email} > no match"
        		end
      		else
        		puts "#{email} > skipped (already has photo)"
      		end
      		File.delete("#{email}.png") if File.exists?("#{email}.png")
    	end
  	rescue Exception => ex
    	puts ex
  	end
end
puts "Updated #{i} contact photos"
